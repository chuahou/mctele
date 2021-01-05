-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mcjoin.Telegram ( BotToken
                       , ChatID
                       , sendServerStatus
                       ) where

import           Control.Monad           (void)
import           Data.String.Interpolate (i)
import           Network.HTTP.Simple     (httpNoBody, parseRequest)

type BotToken = String
type ChatID   = String

-- | @sendServerStatus tok chatId cs@ uses the Telegram Bot API with token @tok@
-- to send message @cs@ to chat with ID @chatId@.
sendServerStatus :: BotToken -> ChatID -> String -> IO ()
sendServerStatus tok chatId cs = request >>= void . httpNoBody
    where
        request = parseRequest
            [i|https://api.telegram.org/bot#{tok}/sendMessage?chat_id=#{chatId}&text=#{cs}|]
