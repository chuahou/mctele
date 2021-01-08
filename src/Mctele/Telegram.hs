-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

{-# LANGUAGE DeriveGeneric #-}

module Mctele.Telegram ( BotToken
                       , ChatID
                       , MessageID
                       , sendServerStatus
                       , deleteMessage
                       ) where

import           Mctele.Server

import           Control.Monad           (void, (>=>))
import           Data.Aeson              (FromJSON)
import           Data.Functor            ((<&>))
import           Data.List               (intercalate)
import           Data.String.Interpolate (i)
import           GHC.Generics            (Generic)
import           Network.HTTP.Simple     (getResponseBody, httpJSONEither,
                                          httpNoBody, parseRequest)

type BotToken  = String
type ChatID    = String
type MessageID = String

-- | @createRequest tok op params@ creates a Telegram API request string with
-- bot token @tok@, operation @op@ and tuples of @(key, value)@s as parameters
-- @params@.
createRequest :: BotToken -> String -> [(String, String)] -> String
createRequest tok op params =
    [i|https://api.telegram.org/bot#{tok}/#{op}?#{params'}|]
    where
        params'        = intercalate "&" . map mkParam $ params
        mkParam (k, v) = [i|#{k}=#{v}|]

-- | @sendServerStatus silent tok chatId info@ uses the Telegram Bot API with
-- token @tok@ to send server status @info@ to chat with ID @chatId@, with
-- silent notifications if @silent@ is true.
--
-- Returns the resulting message ID.
sendServerStatus :: Bool -> BotToken -> ChatID -> ServerInfo -> IO (Maybe MessageID)
sendServerStatus silent tok chatId info =   request >>= httpJSONEither
                                        <&> either (const Nothing) getMessageId
                                        .   getResponseBody
    where
        request = parseRequest $ createRequest tok "sendMessage" $
                    [ ("chat_id",    chatId)
                    , ("text",       text info)
                    , ("parse_mode", "MarkdownV2")
                    ] ++ [("disable_notification", "true") | silent]
        text (Just []) =   "Server *ONLINE* with no players\\."
        text (Just ps) = [i|Server *ONLINE* with #{length ps} players: `#{unwords ps}`\\.|]
        text Nothing   =   "Server *OFFLINE*\\."

-- | Deletes the specified message on Telegram.
deleteMessage :: BotToken -> ChatID -> MessageID -> IO ()
deleteMessage tok chatId msgId = request >>= void . httpNoBody
    where
        request = parseRequest $ createRequest tok "deleteMessage"
                    [ ("chat_id",    chatId)
                    , ("message_id", msgId)
                    ]

-- | Returns 'Just' the message ID for a successful request and 'Nothing'
-- otherwise, with the request response JSON as input.
getMessageId :: TelegramResponse -> Maybe MessageID
getMessageId = result >=> (fmap show . message_id)

-- | Telegram API request response JSON type. We are only interested in the
-- result member.
newtype TelegramResponse = TelegramResponse { result :: Maybe TelegramResult }
    deriving Generic
instance FromJSON TelegramResponse

-- | Telegram API request response result JSON type. We are only interested in
-- the resulting message ID.
newtype TelegramResult = TelegramResult { message_id :: Maybe Int }
    deriving Generic
instance FromJSON TelegramResult
