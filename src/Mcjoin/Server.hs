-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module Mcjoin.Server ( doApiRequest
                     , Player
                     ) where

import           Data.Aeson          (FromJSON)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Network.HTTP.Simple (getResponseBody, httpJSONEither,
                                      parseRequest)

-- JSON types
data ServerInfo = ServerInfo
    { online  :: Bool
    , players :: PlayersInfo
    } deriving (Generic, Show)
newtype PlayersInfo = PlayersInfo { list :: Maybe [Text] }
    deriving (Generic, Show)

-- We use generics to automatically create 'FromJSON' instances.
instance FromJSON ServerInfo
instance FromJSON PlayersInfo

-- | A @Player@ is described solely by their username for our purposes.
type Player = String

-- | @doApiRequest url@ returns @Just@ a list of 'Player's that are online, and
-- @Nothing@ if @url@ is invalid, or the request fails.
doApiRequest :: String -> IO (Maybe ServerInfo)
doApiRequest url = case parseRequest url of
                     Just req -> httpJSONEither req >>= (\case
                        Right info -> pure $ Just info
                        Left  _    -> pure Nothing) . getResponseBody
                     Nothing  -> pure Nothing
