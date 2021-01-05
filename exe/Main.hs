-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Main where

import           Mcjoin.Server
import           Mcjoin.Telegram

import           Data.Char                    (isDigit)
import qualified Network.Socket               as Net
import           System.Environment           (getEnv)
import           Text.ParserCombinators.ReadP
import           Text.Read                    (readMaybe)

main :: IO ()
main = do
    { tok    <- getEnv "MCJOIN_BOT_TOKEN"
    ; chatId <- getEnv "MCJOIN_CHAT_ID"
    ; addr   <- getEnv "MCJOIN_SERVER_ADDR" >>= mkAddr
    ; info   <- getInfo addr
    ; sendServerStatus tok chatId info
    }
    where
        mkAddr :: String -> IO Net.SockAddr
        mkAddr cs = case readP_to_S addrP cs of
                      [(y, "")] -> pure y
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
