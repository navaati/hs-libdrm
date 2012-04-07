module Graphics.KMS.Encoder where

import Foreign
import Foreign.C
import System.Posix
import Control.Monad (liftM5)

#include<stdint.h>
#include<xf86drmMode.h>

import Graphics.KMS.Types
import Graphics.KMS.Utils

data Encoder = Encoder
                  { encoderId :: EncoderId
                  , encoderType :: EncoderType
                  , encoderCrtcId :: CrtcId
                  , encoderPossibleCrtcs :: Word32
                  , encoderPossibleClones :: Word32
                  } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeEncoder, field)

peekEncoder :: Ptr Encoder -> IO Encoder
peekEncoder ptr = liftM5 Encoder
  ((#p encoder_id) ptr)
  ((#p encoder_type) ptr)
  ((#p crtc_id) ptr)
  ((#p possible_crtcs) ptr)
  ((#p possible_clones) ptr)

getEncoder :: (?drm :: Drm) ⇒ EncoderId -> IO Encoder
getEncoder eId = do
  ptr <- throwErrnoIfNull "drmModeGetEncoder" (drmModeGetEncoder ?drm eId)
  encoder <- peekEncoder ptr
  drmModeFreeEncoder ptr
  return encoder


data EncoderType = EncoderNone | DAC | TMDS | EncoderLVDS | TVDAC deriving (Show, Eq)

encoderTypeEnum :: [(Word32,EncoderType)]
encoderTypeEnum = [
  ((#const DRM_MODE_ENCODER_NONE), EncoderNone),
  ((#const DRM_MODE_ENCODER_DAC), DAC),
  ((#const DRM_MODE_ENCODER_TMDS), TMDS),
  ((#const DRM_MODE_ENCODER_LVDS), EncoderLVDS),
  ((#const DRM_MODE_ENCODER_TVDAC), TVDAC)]

instance Storable EncoderType where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek = peekEnum encoderTypeEnum
  poke = undefined

foreign import ccall "drmModeGetEncoder"
  drmModeGetEncoder :: Drm -> EncoderId -> IO (Ptr Encoder)
foreign import ccall "drmModeFreeEncoder"
  drmModeFreeEncoder :: Ptr Encoder -> IO ()
