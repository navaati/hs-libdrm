{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.DRM.KMS.FrameBuffer
       ( fbRes
       , fbPitch
       , fbBPP
       , fbDepth
       , fbHandle
       , addFb
       , rmFb
       ) where

import FunctionalTools.Unicode
import Foreign
import Foreign.C.Error
import Data.Reflection
import Data.Proxy

import System.DRM.C.KMS.FrameBuffer
import System.DRM.Types
import System.DRM.BufferObject

fbRes ∷ (RDrm drm) ⇒ Fb drm → IO (Width,Height)
fbRes = fmap (c'drmModeFB'width &&& c'drmModeFB'height) ∘ getFb

fbPitch ∷ (RDrm drm) ⇒ Fb drm → IO Pitch
fbPitch = fmap c'drmModeFB'pitch ∘ getFb

fbBPP ∷ (RDrm drm) ⇒ Fb drm → IO Word32
fbBPP = fmap c'drmModeFB'bpp ∘ getFb

fbDepth ∷ (RDrm drm) ⇒ Fb drm → IO Word32
fbDepth = fmap c'drmModeFB'depth ∘ getFb

fbHandle ∷ (RDrm drm) ⇒ Fb drm → IO (DrmBOHandle drm)
fbHandle = fmap (DrmBOHandle ∘ c'drmModeFB'handle) ∘ getFb


getFb ∷ (RDrm drm) ⇒
  Fb drm → IO C'drmModeFB
getFb fId = do
  ptr ← throwErrnoIfNull "drmModeGetFB" $
         applyDrm c'drmModeGetFB fId
  fb ← peek ptr
  c'drmModeFreeFB ptr
  return fb

addFb ∷ ∀drm bo. (RDrm drm, ImageBO bo, BOHandle bo ~ DrmBOHandle drm) ⇒ bo → IO (Fb drm)
addFb bo = alloca $ \fIdPtr → do
  let (w,h) = boRes bo
  throwErrnoIfMinus1_ "drmModeAddFB" $ c'drmModeAddFB
    (reflect (Proxy ∷ Proxy drm)) w h (boDepth bo)
    (boBPP bo) (boPitch bo) (boHandle bo) fIdPtr
  peek fIdPtr

rmFb ∷ (RDrm drm) ⇒
  Fb drm → IO ()
rmFb = throwErrnoIfMinus1_ "drmModeRmFB" ∘ applyDrm c'drmModeRmFB
