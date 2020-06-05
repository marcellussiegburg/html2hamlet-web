{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.HomeSpec (spec) where

import Data.FileEmbed                   (embedStringFile)
import Data.List.Extra                  (escapeHTML)
import TestImport

spec :: Spec
spec = withApp $
  describe "Homepage" $ do
    it "loads the index and checks it looks right" $ do
      get HomeR
      statusIs 200
      htmlAnyContain "h1" "HTML2Hamlet"
    it "performs HTML2Hamlet transformations" $ do
      get HomeR
      statusIs 200
      htmlNoneContain ".upload-response" hamlet
      request $ do
        setMethod "POST"
        setUrl HomeR
        addToken
        fileByLabelExact "Choose a file" "test/data/index.html" "text/html"
      htmlAllContain ".upload-response" hamlet

hamlet :: String
hamlet = escapeHTML $(embedStringFile "test/data/index.hamlet")
