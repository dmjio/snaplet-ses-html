{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Snap.Snaplet.SES
    ( -- * Initialization
      initAWSKeys
    , sendSESMailBlaze
    , sendSESMail
      -- * Types
    , AWSKeys (..)
    , module Network.SES
    ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Configurator          as C
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (mempty)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Network.SES
import           Snap
import qualified Text.Blaze.Html5           as H

------------------------------------------------------------------------------
-- | Type to hold AWS Config Information
-- > data App = App { _awsKeys :: Snaplet AWSKeys }
--
data AWSKeys = AWSKeys
    { secretKey :: Text -- ^ AWS Secret Key
    , publicKey :: Text -- ^ AWS Public Key
    , region    :: Text
    , sender    :: Text
    } deriving Show

------------------------------------------------------------------------------
-- | Send Blaze email from a snap handler
sendSESMailBlaze
    :: (MonadState AWSKeys (m b v'), MonadSnaplet m, MonadIO (m b v))
    => SnapletLens v v' -- ^ The lens representing the `AWSKeys` type
    -> To               -- ^ Who the email is intended for
    -> Subject          -- ^ Email Subject
    -> H.Html           -- ^ Email Body
    -> m b v SESResult
sendSESMailBlaze
   keysLens
   to
   subject
   body
    = do AWSKeys secret public region sender <- with keysLens get
         let region' = case region of
                         "us-east-1" -> USEast1
                         "us-west-2" -> USWest2
                         "eu-west-1" -> EUWest1
                         _           -> USEast1
         liftIO $ sendEmailBlaze
                    (PublicKey $ encodeUtf8 public)
                    (SecretKey $ encodeUtf8 secret)
                    region'
                    (BL8.fromStrict $ encodeUtf8 sender)
                    to
                    subject
                    body

------------------------------------------------------------------------------
-- | Send a ByteString of HTML from a Snap Handler
sendSESMail
    :: (MonadState AWSKeys (m b v'), MonadSnaplet m, MonadIO (m b v))
    => SnapletLens v v' -- ^ The lens representing the `AWSKeys` type
    -> To               -- ^ Who the email is intended for
    -> Subject          -- ^ Email Subject
    -> BL8.ByteString   -- ^ Email Body
    -> m b v SESResult
sendSESMail
   keysLens
   to
   subject
   body
    = do AWSKeys secret public region sender <- with keysLens get
         let region' = case region of
                         "us-east-1" -> USEast1
                         "us-west-2" -> USWest2
                         "eu-west-1" -> EUWest1
                         _           -> USEast1
         liftIO $ sendEmail
                    (PublicKey $ encodeUtf8 public)
                    (SecretKey $ encodeUtf8 secret)
                    region'
                    (BL8.fromStrict $ encodeUtf8 sender)
                    to
                    subject
                    body

------------------------------------------------------------------------------
-- | Initialize snaplet
-- > keys <- nestSnaplet "aws" awskeys initAWSKeys
-- > return App { awsKeys = keys }
initAWSKeys :: SnapletInit a AWSKeys
initAWSKeys = makeSnaplet "ses-html" "Get your aws keys" Nothing $ do
        config <- getSnapletUserConfig
        liftIO $ AWSKeys <$> getPublic config
                         <*> getSecret config
                         <*> getRegion config
                         <*> getSender config
  where
    getPublic : getSecret : getRegion : getSender : _ 
         = map getConfig [ "public"
                         , "secret"
                         , "region"
                         , "sender"
                         ]
    getConfig name config = fromMaybe mempty <$> C.lookup config name

