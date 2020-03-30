{-# LANGUAGE OverloadedStrings #-}

module CoreDataImpl
    ( runCoreData
    )
where

import           Core.Data
import           Core.Encoding
import           Core.Program
import           Core.Text
import qualified Data.ByteString.Lazy          as B
import           Data.Maybe

jsValueLookup :: JsonKey -> JsonValue -> Maybe JsonValue
jsValueLookup k (JsonObject m) = lookupKeyValue k m
jsValueLookup _ _              = Nothing

jsValueMap :: (JsonValue -> Maybe JsonValue) -> JsonValue -> Maybe JsonValue
jsValueMap f (JsonArray a) = Just (JsonArray (mapMaybe f a))
jsValueMap _ _             = Nothing

manipulate :: B.ByteString -> Maybe JsonValue
manipulate bstr = do
    jsValue <- decodeFromUTF8 $ intoBytes bstr
    requires <- jsValueLookup "__requires" jsValue
    jsValueMap (jsValueLookup "name") requires

processFile :: FilePath -> IO (Maybe JsonValue)
processFile path = do
    contents <- B.readFile path
    return $ manipulate contents

runCoreData :: FilePath -> Program None ()
runCoreData path = withContext $ \runProgram -> do
    extract <- processFile path
    case extract of
        Nothing -> return ()
        Just v  -> runProgram $ writeR v
