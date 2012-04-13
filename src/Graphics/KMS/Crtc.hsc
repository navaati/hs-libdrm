module Graphics.KMS.Crtc where

import Prelude.Unicode
import Foreign
import Foreign.C
import System.Posix
import Data.Reflection
import Data.Proxy

#include<stdint.h>
#include<xf86drmMode.h>

import Graphics.KMS.ModeInfo
import Graphics.KMS.Types

data Crtc drm = ConnectedCrtc
                { crtcId ∷ CrtcId drm
                , crtcFbId ∷ FbId drm
                , crtcPosition ∷ (Word32,Word32)
                , crtcPxSize ∷ (Word32,Word32)
                , crtcMode ∷ ModeInfo
                , crtcGammaSize ∷ Int
                } |
                DisconnectedCrtc
                { crtcId ∷ CrtcId drm
                , crtcPosition ∷ (Word32,Word32)
                , crtcPxSize ∷ (Word32,Word32)
                , crtcGammaSize ∷ Int
                } deriving (Show)
              
#define hsc_p(field) hsc_peek(drmModeCrtc, field)

peekCrtc ∷ CrtcPtr drm → IO (Crtc drm)
peekCrtc ptr = do
  cId ← (#p crtc_id) ptr
  fbId ← (#p buffer_id) ptr
  [x, y, w, h] ←
    mapM ($ ptr)
    [ (#p x), (#p y)
    , (#p width), (#p height)]
  gammaSize ← (#p gamma_size) ptr
  if fbId ≡ 0
    then return $ DisconnectedCrtc cId (x,y) (w,h) gammaSize
    else do
    modeValid ← fmap toBool ((#p mode_valid) ptr ∷ IO CInt)
    if modeValid then return () else error "Connected CRTC mode not valid"
    mode ← (#p mode) ptr
    return $ ConnectedCrtc cId (FbId fbId) (x,y) (w,h) mode gammaSize

getCrtc ∷ ∀drm. (drm `Reifies` Drm) ⇒
           CrtcId drm → IO (Crtc drm)
getCrtc cId = do
  ptr ← throwErrnoIfNull "drmModeGetCrtc" $
         drmModeGetCrtc (reflect (Proxy ∷ Proxy drm)) cId
  crtc ← peekCrtc ptr
  drmModeFreeCrtc ptr
  return crtc

type CrtcPtr drm = Ptr (Crtc drm)

foreign import ccall "drmModeGetCrtc"
  drmModeGetCrtc ∷ Drm → CrtcId drm → IO (CrtcPtr drm)
foreign import ccall "drmModeFreeCrtc"
  drmModeFreeCrtc ∷ CrtcPtr drm → IO ()
