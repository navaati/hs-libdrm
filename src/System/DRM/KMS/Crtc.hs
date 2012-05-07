{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module System.DRM.KMS.Crtc
       ( crtcFb
       , crtcPosition
       , crtcPxSize
       , crtcMode
       , crtcGammaSize
       , setCrtc
       ) where

import FunctionalTools.Unicode
import Foreign
import Foreign.C.Error
import Data.List(genericLength)

import System.DRM.C.KMS.Crtc
import System.DRM.KMS.ModeInfo
import System.DRM.C.KMS.ModeInfo
import System.DRM.Types

crtcFb ∷ (RDrm drm) ⇒ Crtc drm → IO (Maybe (FbId drm))
crtcFb = fmap (fbToMaybe ∘ c'drmModeCrtc'buffer_id) ∘ getCrtc
  where fbToMaybe buffer_id = if buffer_id ≡ 0 then Nothing else Just $ FbId buffer_id

crtcPosition ∷ (RDrm drm) ⇒ Crtc drm → IO (Word32,Word32)
crtcPosition = fmap (c'drmModeCrtc'x &&& c'drmModeCrtc'y)
               ∘ getCrtc

crtcPxSize ∷ (RDrm drm) ⇒ Crtc drm → IO (Width,Height)
crtcPxSize = fmap (c'drmModeCrtc'width &&& c'drmModeCrtc'height)
             ∘ getCrtc

crtcMode ∷ (RDrm drm) ⇒ Crtc drm → IO (Maybe ModeInfo)
crtcMode =
  getCrtc ⋙
  fmap (\cCrtc →
         if toBool $ c'drmModeCrtc'mode_valid cCrtc
         then Just ∘ cToModeInfo $ c'drmModeCrtc'mode cCrtc
         else Nothing)

crtcGammaSize ∷ (RDrm drm) ⇒ Crtc drm → IO Int
crtcGammaSize = fmap (fromIntegral ∘ c'drmModeCrtc'gamma_size) ∘ getCrtc

getCrtc ∷ (RDrm drm) ⇒
           Crtc drm → IO C'drmModeCrtc
getCrtc cId = do
  ptr ← throwErrnoIfNull "drmModeGetCrtc" $
         applyDrm c'drmModeGetCrtc cId
  crtc ← peek ptr
  c'drmModeFreeCrtc ptr
  return crtc

setCrtc ∷ (RDrm drm) ⇒
  Crtc drm → FbId drm → (Word32,Word32) → [Connector drm] → ModeInfo → IO ()
setCrtc cId fId (x,y) connectors mode =
  throwErrnoIfMinus1_ "drmModeSetCrtc" $
  withArray connectors $
  \connA → with (modeInfoToC mode) $
   \modeP →
    applyDrm c'drmModeSetCrtc cId fId x y
    connA (genericLength connectors) modeP
