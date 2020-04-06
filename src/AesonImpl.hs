{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AesonImpl
    ( runAeson
    )
where

import           Core.Program
import           Core.System
import           Core.Text
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy          as B
import           Data.Text
import           GHC.Generics


data Requires = Requires {
      type_ :: Text
    , id_ :: Text
    , name_ :: Text
    , version_ :: Text
    } deriving (Generic, Show)

instance FromJSON Requires where
    parseJSON =
        withObject "Requires"
            $ \v ->
                  Requires
                      <$> v
                      .:  "type"
                      <*> v
                      .:  "id"
                      <*> v
                      .:  "name"
                      <*> v
                      .:  "version"

instance ToJSON Requires where
    toEncoding (Requires type__ id__ name__ version__) =
        pairs
            $  "type"
            .= type__
            <> "id"
            .= id__
            <> "name"
            .= name__
            <> "version"
            .= version__

data Dashboard = Dashboard {
      __inputs :: [Int]
    , __requires :: [Requires]
    , editable :: Bool
    , gnetId :: Maybe Text
    , graphTooltip :: Int
    , id :: Maybe Text
    , iteration :: Int
    , links :: [Text]
    } deriving (Generic, Show)

instance FromJSON Dashboard

instance ToJSON Dashboard where
    toEncoding = genericToEncoding defaultOptions

manipulate :: B.ByteString -> Maybe B.ByteString
manipulate bstr = do
    dashboard <- decode bstr
    let requires = __requires dashboard
    let names    = Prelude.map name_ requires
    return $ encodePretty names

processFile :: FilePath -> IO (Maybe B.ByteString)
processFile path = do
    contents <- B.readFile path
    return $ manipulate contents

runAeson :: FilePath -> Program None ()
runAeson path = do
    extract <- liftIO $ processFile path
    case extract of
        Nothing -> return ()
        Just v  -> write $ intoRope v
