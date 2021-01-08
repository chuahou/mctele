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
import           Data.ByteString.Char8   (pack)
import           Data.Functor            ((<&>))
import           Data.List               (intercalate)
import           Data.String.Interpolate (i)
import           GHC.Generics            (Generic)
import           Network.HTTP.Simple     (Request, getResponseBody,
                                          httpJSONEither, httpNoBody,
                                          parseRequest, setRequestQueryString)

type BotToken  = String
type ChatID    = String
type MessageID = String

-- | @createRequest tok op params@ creates a Telegram API request with bot token
-- @tok@, operation @op@ and tuples of @(key, value)@s as parameters @params@.
createRequest :: BotToken -> String -> [(String, String)] -> IO Request
createRequest tok op params = setRequestQueryString query <$> baseRequest
    where
        baseRequest    = parseRequest
                            [i|https://api.telegram.org/bot#{tok}/#{op}|]
        query          = map mkParam params
        mkParam (k, v) = (pack k, pure . pack $ v)

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
        request = createRequest tok "sendMessage" $
                    [ ("chat_id",    chatId)
                    , ("text",       text info)
                    , ("parse_mode", "MarkdownV2")
                    ] ++ [("disable_notification", "true") | silent]
        text (Just []) =   "\\[Server\\] *ONLINE*"
        text (Just ps) = [i|\\[Server\\] *ONLINE*, #{length ps} players: `#{intercalate "`, `" ps}`|]
        text Nothing   =   "\\[Server\\] *OFFLINE*"

-- | Deletes the specified message on Telegram.
deleteMessage :: BotToken -> ChatID -> MessageID -> IO ()
deleteMessage tok chatId msgId = request >>= void . httpNoBody
    where
        request = createRequest tok "deleteMessage"
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
