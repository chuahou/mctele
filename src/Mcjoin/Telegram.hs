-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mcjoin.Telegram ( BotToken
                       , ChatID
                       , sendServerStatus
                       ) where

import           Mcjoin.Server

import           Control.Monad           (void)
import           Data.String.Interpolate (i)
import           Network.HTTP.Simple     (httpNoBody, parseRequest)

type BotToken = String
type ChatID   = String

-- | @sendServerStatus tok chatId info@ uses the Telegram Bot API with token @tok@
-- to send server status @info@ to chat with ID @chatId@.
sendServerStatus :: BotToken -> ChatID -> ServerInfo -> IO ()
sendServerStatus tok chatId info = request >>= void . httpNoBody
    where
        request = parseRequest
            [i|https://api.telegram.org/bot#{tok}/sendMessage?chat_id=#{chatId}&text=#{text info}&parse_mode=MarkdownV2|]
        text (Just []) = "Server is *online* with no players\\."
        text (Just ps) = [i|Server is *online* with players: *#{unwords ps}*\\.|]
        text Nothing   = "Server is *offline*\\."
