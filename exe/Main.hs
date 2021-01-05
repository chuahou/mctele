-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Main where

import           Mcjoin.Server
import           Mcjoin.Telegram

import           System.Environment (getEnv)

main :: IO ()
main = do
    { tok       <- getEnv "MCJOIN_BOT_TOKEN"
    ; chatId    <- getEnv "MCJOIN_CHAT_ID"
    ; Just info <- getEnv "MCJOIN_SERVER_ADDR" >>= doApiRequest
    ; sendServerStatus tok chatId info
    }
