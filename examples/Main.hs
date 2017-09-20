{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main ( main ) where

import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Snap
import           Snap.Snaplet.SES

data App = App {
   _awsKeys :: Snaplet AWSKeys
}

makeLenses ''App

initApp :: SnapletInit App App
initApp = makeSnaplet "name" "description" Nothing $ do
            _awsKeys <- nestSnaplet "ses-html" awsKeys initAWSKeys
	    addRoutes [("/", handleKeys)]
	    return App {..}
  where
    handleKeys = method GET $ do
      with awsKeys $ withKeys $ liftIO . print
      result <- with awsKeys $ sendEmail ["david@solidtranslate.com"] "cookie-crisp" "<h1>TEST</h1>"
      liftIO $ print result
      writeBS "done"

main :: IO ()
main = do (_, app, _) <- runSnaplet Nothing initApp
          httpServe config app
  where
    config = setAccessLog ConfigNoLog $
             setErrorLog ConfigNoLog $
             defaultConfig





