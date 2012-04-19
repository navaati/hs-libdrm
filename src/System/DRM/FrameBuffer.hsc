{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.DRM.FrameBuffer where

import Foreign
import Foreign.C
import System.Posix
import Data.Reflection
import Data.Proxy

#include<stdint.h>
#include<xf86drmMode.h>

import System.DRM.Types
import System.DRM.BufferObject

data Fb drm = Fb
              { fbId ∷ FbId drm
              , fbRes ∷ (Width,Height)
              , fbPitch ∷ Pitch
              , fbBPP, fbDepth ∷ Word32
              , fbHandle ∷ BOHandle drm
              } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeFB, field)

peekFb ∷ FbPtr drm → IO (Fb drm)
peekFb ptr = do
  fb_id ← (#p fb_id) ptr
  [w,h,pitch,bpp,depth] ← mapM ($ ptr)
    [(#p width)
    ,(#p height)
    ,(#p pitch)
    ,(#p bpp)
    ,(#p depth)]
  handle ← (#p handle) ptr
  return $ Fb fb_id (w,h) pitch bpp depth handle

getFb ∷ (RDrm drm) ⇒
  FbId drm → IO (Fb drm)
getFb fId = do
  ptr ← throwErrnoIfNull "drmModeGetFB" $
         applyDrm drmModeGetFB fId
  fb ← peekFb ptr
  drmModeFreeFB ptr
  return fb

addFb ∷ ∀drm bo. (RDrm drm, ImageBO bo drm) ⇒ bo → IO (FbId drm)
addFb bo = alloca $ \fIdPtr → do
  let (w,h) = boRes bo
  throwErrnoIfMinus1_ "drmModeAddFB" $ drmModeAddFB
    (reflect (Proxy ∷ Proxy drm)) w h (boDepth bo) 
    (boBPP bo) (boPitch bo) (boHandle bo) fIdPtr
  peek fIdPtr

type FbPtr drm = Ptr (Fb drm)

foreign import ccall "drmModeGetFB"
  drmModeGetFB ∷ Drm → FbId drm → IO (FbPtr drm)
foreign import ccall "drmModeFreeFB"
  drmModeFreeFB ∷ FbPtr drm → IO ()
foreign import ccall "drmModeAddFB"
  drmModeAddFB ∷ Drm → Width → Height → Depth → BPP → Pitch
  → BOHandle drm → Ptr (FbId drm) → IO CInt
