module System.DRM.KMS.Encoder where

import Foreign
import Foreign.C
import System.Posix
import Control.Monad (liftM5)
import Data.Reflection
import Data.Proxy

#include<stdint.h>
#include<xf86drmMode.h>

import System.DRM.Types
import System.DRM.FFIUtils

data Encoder drm = Encoder
                   { encoderId ∷ EncoderId drm
                   , encoderType ∷ EncoderType
                   , encoderCrtcId ∷ CrtcId drm
                   , encoderPossibleCrtcs ∷ Word32
                   , encoderPossibleClones ∷ Word32
                   } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeEncoder, field)

peekEncoder ∷ EncoderPtr drm → IO (Encoder drm)
peekEncoder ptr = liftM5 Encoder
  ((#p encoder_id) ptr)
  ((#p encoder_type) ptr)
  ((#p crtc_id) ptr)
  ((#p possible_crtcs) ptr)
  ((#p possible_clones) ptr)

getEncoder ∷ ∀drm. (drm `Reifies` Drm) ⇒
              EncoderId drm → IO (Encoder drm)
getEncoder eId = do
  ptr ← throwErrnoIfNull "drmModeGetEncoder" $
         drmModeGetEncoder (reflect (Proxy ∷ Proxy drm)) eId
  encoder ← peekEncoder ptr
  drmModeFreeEncoder ptr
  return encoder


data EncoderType = EncoderNone | DAC | TMDS | EncoderLVDS | TVDAC deriving (Show, Eq)

encoderTypeEnum ∷ [(Word32,EncoderType)]
encoderTypeEnum = [
  ((#const DRM_MODE_ENCODER_NONE), EncoderNone),
  ((#const DRM_MODE_ENCODER_DAC), DAC),
  ((#const DRM_MODE_ENCODER_TMDS), TMDS),
  ((#const DRM_MODE_ENCODER_LVDS), EncoderLVDS),
  ((#const DRM_MODE_ENCODER_TVDAC), TVDAC)]

instance Storable EncoderType where
  sizeOf _ = sizeOf (undefined ∷ Word32)
  alignment _ = alignment (undefined ∷ Word32)
  peek = peekEnum encoderTypeEnum
  poke = undefined

type EncoderPtr drm = Ptr (Encoder drm)

foreign import ccall "drmModeGetEncoder"
  drmModeGetEncoder ∷ Drm → EncoderId drm → IO (EncoderPtr drm)
foreign import ccall "drmModeFreeEncoder"
  drmModeFreeEncoder ∷ EncoderPtr drm → IO ()
