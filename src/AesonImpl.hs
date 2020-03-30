{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AesonImpl
    ( runAeson
    )
where

import           Core.Program
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
    toEncoding (Requires type_ id_ name_ version_) =
        pairs
            $  "type"
            .= type_
            <> "id"
            .= id_
            <> "name"
            .= name_
            <> "version"
            .= version_

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

instance FromJSON Dashboard where
    parseJSON = withObject "Dashboard" $ \v ->
        Dashboard
            <$> v
            .:  "__inputs"
            <*> v
            .:  "__requires"
            <*> v
            .:  "editable"
            <*> v
            .:  "gnetId"
            <*> v
            .:  "graphTooltip"
            <*> v
            .:  "id"
            <*> v
            .:  "iteration"
            <*> v
            .:  "links"

instance ToJSON Dashboard where
    toEncoding (Dashboard __inputs __requires editable gnetId graphTooltip id iteration links)
        = pairs
            $  "__inputs"
            .= __inputs
            <> "__requires"
            .= __requires
            <> "editable"
            .= editable
            <> "gnetId"
            .= gnetId
            <> "graphTooltip"
            .= graphTooltip
            <> "id"
            .= id
            <> "iteration"
            .= iteration
            <> "links"
            .= links

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
runAeson path = withContext $ \runProgram -> do
    extract <- processFile path
    case extract of
        Nothing -> return ()
        Just v  -> runProgram $ write $ intoRope v
