{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.SES
    ( -- * Types
      AWSKeys    (..)
    , HasAWSKeys (..)
      -- * Initialization
    , initAWSKeys
    , sendEmail
    , sendEmailBlaze
    , withKeys
    , module Network.SES
    ) where

import           Control.Monad.Trans.Reader (ReaderT, asks, ask)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Configurator          as C
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mempty)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Network.SES                hiding (sendEmail, sendEmailBlaze)
import qualified Network.SES                as SES
import           Snap
import qualified Text.Blaze.Html5           as H

------------------------------------------------------------------------------
-- | Type to hold AWS Config Information
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE RecordWildCards   #-}
-- > {-# LANGUAGE TemplateHaskell   #-}
-- > module Main ( main ) where
-- >  
-- > import           Control.Lens
-- > import qualified Data.ByteString.Char8 as B8
-- > import qualified Data.ByteString.Lazy.Char8 as BL8
-- > import           Snap
-- > import           Snap.Snaplet.SES
-- >  
-- > data App = App {
-- >    _awsKeys :: Snaplet AWSKeys
-- > }
-- >  
-- > makeLenses ''App
-- >  
-- > initApp :: SnapletInit App App
-- > initApp = makeSnaplet "name" "description" Nothing $ do
-- >             _awsKeys <- nestSnaplet "ses-html" awsKeys initAWSKeys
-- >             addRoutes [("/", handleKeys)]
-- >             return App {..}
-- >   where
-- >     handleKeys = method GET $ do
-- >       with awsKeys $ withKeys $ liftIO . print
-- >       result <- with awsKeys $ sendEmail ["david@solidtranslate.com"] "cookie-crisp" "<h1>TEST</h1>"
-- >       liftIO $ print result
-- >       writeBS "done"
-- >  
-- > main :: IO ()
-- > main = do (_, app, _) <- runSnaplet Nothing initApp
-- >           httpServe config app
-- >   where
-- >     config = setAccessLog ConfigNoLog $
-- >              setErrorLog ConfigNoLog $
-- >              defaultConfig
--
-- .\/snaplets\/ses-html\/devel.cfg
--
-- > public = "publickey"
-- > secret = "secretkey"
-- > region = "us-east-1"
-- > sender = "sender@verifiedaddress.com"
--

data AWSKeys = AWSKeys
    { publicKey :: PublicKey -- ^ AWS Public Key
    , secretKey :: SecretKey -- ^ AWS Secret Key
    , region    :: Region    -- ^ AWS Region Key
    , sender    :: Text      -- ^ AWS Verified Sender Email
    } deriving Show

------------------------------------------------------------------------------
-- | Class to allow extraction of `AWSKeys` from arbitrary Monads constrained by MonadIO
class MonadIO m => HasAWSKeys m where
    getKeys :: m AWSKeys

instance HasAWSKeys (Handler b AWSKeys) where
    getKeys = get

instance MonadIO m => HasAWSKeys (ReaderT (Snaplet AWSKeys) m) where
    getKeys = asks (^# snapletValue)

instance MonadIO m => HasAWSKeys (ReaderT AWSKeys m) where
    getKeys = ask

------------------------------------------------------------------------------
-- | Helper function for operating on `AWSKeys` inside of `HasAWSKeys` constrained Monads
withKeys
 :: HasAWSKeys m 
 => (AWSKeys -> IO a) -- ^ Function operating on Keys
 -> m a    
withKeys f = getKeys >>= liftIO . f 

------------------------------------------------------------------------------
-- | Send Blaze email from a snap handler
sendEmailBlaze
    :: HasAWSKeys m
    => To               -- ^ Who the email is intended for
    -> Subject          -- ^ Email `Subject`
    -> H.Html           -- ^ Email Body
    -> m SESResult
sendEmailBlaze
   to
   subject
   body = withKeys $ \(AWSKeys secret public region sender) ->
                          SES.sendEmailBlaze
                          secret
                          public
                          region
                          (BL8.fromStrict $ encodeUtf8 sender)
                          to
                          subject
                          body

------------------------------------------------------------------------------
-- | Send a ByteString of HTML from a Snap Handler
sendEmail
    :: HasAWSKeys m
    => To               -- ^ Who the email is intended for
    -> Subject          -- ^ email `Subject`
    -> BL8.ByteString   -- ^ email Body
    -> m SESResult
sendEmail
   to
   subject
   body = withKeys $ \(AWSKeys secret public region sender) ->
                          SES.sendEmail
                          secret
                          public
                          region
                          (BL8.fromStrict $ encodeUtf8 sender)
                          to
                          subject
                          body

------------------------------------------------------------------------------
-- | Initialize snaplet
initAWSKeys :: SnapletInit a AWSKeys
initAWSKeys = makeSnaplet "ses-html" "Get your aws keys" Nothing $ do
        config <- getSnapletUserConfig
        liftIO $ AWSKeys <$> ((PublicKey . encodeUtf8) <$> getPublic config)
                         <*> ((SecretKey . encodeUtf8) <$> getSecret config)
                         <*> getRegion config
                         <*> getSender config
  where
    getRegion config = do
        let f :: Text -> Region
            f x | x == "us-east-1" = USEast1
                | x == "us-west-2" = USWest2
                | x == "eu-west-1" = EUWest1
                | otherwise        = USEast1
        fromMaybe USEast1 <$> fmap f <$> C.lookup config "region"

    getPublic : getSecret : getSender : _
         = map getConfig [ "public"
                         , "secret"
                         , "sender"
                         ]
    getConfig name config = fromMaybe mempty <$> C.lookup config name

