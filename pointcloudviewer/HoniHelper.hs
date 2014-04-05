{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module HoniHelper
  ( takeDepthSnapshot
  , withHoni
  ) where

import qualified Data.ByteString as BS
import           Data.Vector.Storable (Vector)
import           Data.Word (Word16)
import           Honi
import           Honi.Types
import           Data.Vector.Storable.ByteString (byteStringToVector)


-- | Take a snapshot from the depth cam.
--
-- Don't forget to initialize OpenNI first, using `withHoni` or `initialize`.
takeDepthSnapshot :: IO (Either String (Vector Word16, (Int, Int)))
takeDepthSnapshot = do
  -- A little bit dirty, but double initialization is allowed:
  -- https://github.com/OpenNI/OpenNI2/blob/33355e/Source/Core/OniContext.cpp#L50
  initialize oniApiVersion >>= \case
    s | s /= StatusOK -> return $ Left ("initialize failed")
    _                 -> do
      getDeviceList `orWarn` \case
        []   -> return $ Left "No depth device"
        di:_ -> deviceOpenInfo di `orWarn` \d -> do
          deviceCreateStream d SensorDepth `orWarn` \stream -> do
            streamStart stream >>= \case
              s | s /= StatusOK -> return $ Left ("streamStart failed")
              _                 -> do
                streamReadFrame stream `orWarn` \OniFrame{ frameData, frameWidth, frameHeight } -> do
                  let frameVec = bsToVector16Bits frameData
                  return $ Right (frameVec, (frameWidth, frameHeight))

  where
    orWarn :: Oni a -> (a -> IO (Either String b)) -> IO (Either String b)
    orWarn oni f = oni >>= \case
      Left err -> return $ Left (show err)
      Right a  -> f a


bsToVector16Bits :: BS.ByteString -> Vector Word16
bsToVector16Bits = byteStringToVector


-- | Wrap an action in `initialize` and `shutdown`.
withHoni :: IO a -> IO a
withHoni f = do
  initialize oniApiVersion
  x <- f
  shutdown
  return x
