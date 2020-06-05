{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Code where

import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Encoding as LT

import Import
import Data.Aeson

import Html2Hamlet (transformHamlet)

newtype Code = Code { raw :: LT.Text }

instance ToJSON Code where
  toJSON (Code t) = object ["raw" .= t]
instance FromJSON Code where
  parseJSON = withObject "Code" $ \o -> Code <$> o .: "raw"

postCodeR :: Handler Value
postCodeR = do
  code <- requireCheckJsonBody :: Handler Code
  returnJson code {
      raw = transformHamlet $ encodeUtf8 $ raw code
    }
