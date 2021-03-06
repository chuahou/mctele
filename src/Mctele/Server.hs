-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mctele.Server ( getInfo
                     , ServerInfo
                     ) where

import qualified Control.Exception          as Exception
import           Data.Attoparsec.ByteString (Parser, anyWord8, manyTill,
                                             maybeResult, parse, string,
                                             takeWhile1, word8)
import           Data.ByteString            (ByteString, concat, pack)
import qualified Data.ByteString.Char8      as C8
import           Data.Functor               (($>))
import           Data.Int                   (Int32)
import           Data.Serialize             (encode)
import           Data.String.Interpolate    (i)
import           Data.Word                  (Word8)
import qualified Network.Socket             as Net
import qualified Network.Socket.ByteString  as Net
import           System.Timeout             (timeout)
import           Text.Read                  (readMaybe)

-- | Contains all information we need about a server. @Nothing@ if server is
-- offline; @Just@ list of player usernames otherwise.
type ServerInfo = Maybe [Player]

-- | Players are represented by their username string.
type Player = String

-- | Creates a packet with given type and payload.
makePacket :: Word8 -> ByteString -> ByteString
makePacket typ payload = Data.ByteString.concat
                            [ pack [ 0xFE, 0xFD ] -- magic prefix
                            , pack [ typ ]
                            , pack [ 0x00, 0x00, 0x00, 0x00 ] -- session ID
                            , payload
                            ]

-- | Given a socket address, get the server at that address's information.
getInfo :: Net.SockAddr -> IO ServerInfo
getInfo addr = tryGetInfo attempts
    where
        attempts  = 10 :: Int
        timeoutMs = 1000000

        tryGetInfo 0 = pure Nothing -- give up
        tryGetInfo n = timeout timeoutMs (Exception.try handshake
                        :: IO (Either Exception.IOException ServerInfo))
                     >>= maybe tryAgain (\case             -- timeout
                            Left _        -> tryAgain      -- IOException
                            Right Nothing -> tryAgain      -- unsuccessful query
                            Right info    -> succeed info) -- successful query
            where
                tryAgain =  putStrLn [i|Trying again, #{n - 1} attempt(s) left|]
                         >> tryGetInfo (n - 1)
                succeed = (putStrLn "Successful query" $>)

        handshake = do
            { sock <- Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
            ; Net.connect sock addr
            ; Net.sendAll sock (makePacket 0x09 (pack []))
            ; response <- Net.recv sock 128
            ; case (readMaybe (init . drop 5 . C8.unpack $ response)
                    :: Maybe Int32) of
                Just challenge -> request challenge sock <* Net.close sock
                Nothing        -> Nothing                <$ Net.close sock
            }

        request challenge sock = do
            { Net.connect sock addr
            ; Net.sendAll sock (makePacket 0x00 (Data.ByteString.concat
                                    [ encode challenge
                                    , pack (replicate 4 0x00) -- padding
                                    ]))
            ; response <- Net.recv sock 1024
            ; case maybeResult $ parse playersP response of
                Nothing -> pure Nothing
                Just ps -> pure $ Just ps
            }

        -- Parses full stats and returns list of players.
        playersP :: Parser [Player]
        playersP =  manyTill anyWord8 (string padding)
                 >> map C8.unpack <$>
                    manyTill (takeWhile1 (/= 0x00) <* word8 0x00) (word8 0x00)
            where
                padding = pack [ 0x01, 0x70, 0x6C, 0x61, 0x79
                               , 0x65, 0x72, 0x5F, 0x00, 0x00
                               ]
