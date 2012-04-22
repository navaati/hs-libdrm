{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.DRM.FrameBuffer (Fb(..),getFb,addFb,rmFb) where

import Prelude.Unicode
import Foreign
import Foreign.C.Error
import Data.Reflection
import Data.Proxy

import System.DRM.C.FrameBuffer
import System.DRM.Types
import System.DRM.BufferObject

data Fb drm = Fb
              { fbId ∷ FbId drm
              , fbRes ∷ (Width,Height)
              , fbPitch ∷ Pitch
              , fbBPP, fbDepth ∷ Word32
              , fbHandle ∷ BOHandle drm
              } deriving (Show)

cToFb ∷ C'drmModeFB → Fb drm
cToFb (C'drmModeFB{..}) =
  Fb
  (FbId c'drmModeFB'fb_id)
  (c'drmModeFB'width,c'drmModeFB'height)
  c'drmModeFB'pitch
  c'drmModeFB'bpp
  c'drmModeFB'depth
  (BOHandle c'drmModeFB'handle)

getFb ∷ (RDrm drm) ⇒
  FbId drm → IO (Fb drm)
getFb fId = do
  ptr ← throwErrnoIfNull "drmModeGetFB" $
         applyDrm c'drmModeGetFB fId
  fb ← peek ptr
  c'drmModeFreeFB ptr
  return $ cToFb fb

addFb ∷ ∀drm bo. (RDrm drm, ImageBO bo drm) ⇒ bo → IO (FbId drm)
addFb bo = alloca $ \fIdPtr → do
  let (w,h) = boRes bo
  throwErrnoIfMinus1_ "drmModeAddFB" $ c'drmModeAddFB
    (reflect (Proxy ∷ Proxy drm)) w h (boDepth bo)
    (boBPP bo) (boPitch bo) (boHandle bo) fIdPtr
  peek fIdPtr

rmFb ∷ (RDrm drm) ⇒
  FbId drm → IO ()
rmFb = throwErrnoIfMinus1_ "drmModeRmFB" ∘ applyDrm c'drmModeRmFB
