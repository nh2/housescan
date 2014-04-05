{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module HoniHelper
  ( takeDepthSnapshot
  , withHoni
  ) where

import           Control.Applicative
import qualified Data.ByteString as BS
import           Data.ByteString.Internal (toForeignPtr)
import           Data.Vector.Storable (Vector, (!), unsafeFromForeignPtr0)
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable.Internal (updPtr)
import           Data.Word (Word16)
import           Foreign.ForeignPtr (castForeignPtr)
import           Foreign.Marshal.Array (advancePtr)
import           Honi
import           Honi.Types
import           System.IO (hPutStrLn, stderr)


-- | Take a snapshot from the depth cam.
--
-- Don't forget to initialize OpenNI first, using `withHoni` or `initialize`.
takeDepthSnapshot :: IO (Either String (Vector Word16, (Int, Int)))
takeDepthSnapshot = do
  -- A little bit dirty, but double initialization is allowed:
  -- https://github.com/OpenNI/OpenNI2/blob/33355e/Source/Core/OniContext.cpp#L50
  initialize oniApiVersion

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
bsToVector16Bits bs = unsafeFromForeignPtr0 (castForeignPtr fp0) len16Bits
  where
    len16Bits = len `quot` 2
    (fp, off, len)  = toForeignPtr bs
    fp0 | off /= 0  = updPtr (`advancePtr` off) fp
        | otherwise = fp


-- | Wrap an action in `initialize` and `shutdown`.
withHoni :: IO a -> IO a
withHoni f = do
  initialize oniApiVersion
  x <- f
  shutdown
  return x
