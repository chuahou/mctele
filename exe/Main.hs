-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Main where

import           Mcjoin.Server
import           Mcjoin.Telegram

import           Control.Concurrent           (threadDelay)
import           Control.Monad                (when)
import           Data.Char                    (isDigit)
import           Data.Functor                 ((<&>))
import           Data.Maybe                   (fromMaybe)
import qualified Network.Socket               as Net
import           System.Environment           (getEnv, lookupEnv)
import           Text.ParserCombinators.ReadP
import           Text.Read                    (readMaybe)

main :: IO ()
main = do
    { tok      <- getEnv    "MCJOIN_BOT_TOKEN"
    ; chatId   <- getEnv    "MCJOIN_CHAT_ID"
    ; addr     <- lookupEnv "MCJOIN_SERVER_ADDR"    <&> maybe localhost mkAddr
    ; interval <- lookupEnv "MCJOIN_QUERY_INTERVAL" <&> fromMaybe 15
                                                    . (>>= readMaybe)
    ; loop tok chatId addr interval Nothing
    }
    where
        localhost :: Net.SockAddr
        localhost = Net.SockAddrInet 25565 (Net.tupleToHostAddress
                                                (127, 0, 0, 1))

        mkAddr :: String -> Net.SockAddr
        mkAddr cs = case readP_to_S addrP cs of
                      [(y, "")] -> y
                      _         -> error "Invalid server address"

        addrP :: ReadP Net.SockAddr
        addrP = do
            { n1   <- numberP <* char '.'
            ; n2   <- numberP <* char '.'
            ; n3   <- numberP <* char '.'
            ; n4   <- numberP <* char ':'
            ; port <- numberP <* eof
            ; pure $ Net.SockAddrInet port (Net.tupleToHostAddress
                                                (n1, n2, n3, n4))
            }
            where
                numberP :: Read a => ReadP a
                numberP =   many1 (satisfy isDigit)
                        >>= \cs -> case readMaybe cs of
                                     Just w  -> pure w
                                     Nothing -> fail "Invalid number"

        loop tok chatId addr interval = go
            where
                go prev = do
                    { info <- getInfo addr
                    ; when (prev /= info) $ sendServerStatus tok chatId info
                    ; threadDelay (1000000 * interval)
                    ; go info
                    }
