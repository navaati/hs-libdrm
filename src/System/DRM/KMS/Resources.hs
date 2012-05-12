{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module System.DRM.KMS.Resources (Resources(..), getResources) where

import FunctionalTools.Unicode
import Foreign
import Foreign.C
import Data.Reflection
import Data.Proxy

import System.DRM.C.KMS.Resources
import System.DRM.Types

data Resources drm = Resources
                     { resFbs ∷ [FbId drm]
                     , resCrtcs ∷ [CrtcId drm]
                     , resConnectors ∷ [ConnectorId drm]
                     , resEncoders ∷ [EncoderId drm]
                     , resMinSize, resMaxSize ∷ (Width, Height)
                     } deriving (Show)

cToResources ∷ C'drmModeRes → IO (Resources drm)
cToResources (C'drmModeRes{..}) = do
  let fI = fromIntegral
  fbs ← peekArray
    (fI c'drmModeRes'count_fbs) c'drmModeRes'fbs
  crtcs ← peekArray
    (fI c'drmModeRes'count_crtcs) c'drmModeRes'crtcs
  connectors ← peekArray
    (fI c'drmModeRes'count_connectors) c'drmModeRes'connectors
  encoders ← peekArray
    (fI c'drmModeRes'count_encoders) c'drmModeRes'encoders
  return $ Resources
    (fmap FbId fbs)
    (fmap CrtcId crtcs)
    (fmap ConnectorId connectors)
    (fmap EncoderId encoders)
    (c'drmModeRes'min_width,c'drmModeRes'min_height)
    (c'drmModeRes'max_width,c'drmModeRes'max_height)

getResources ∷ ∀drm. (RDrm drm) ⇒
                IO (Resources drm)
getResources = do
  ptr ← throwErrnoIfNull "drmModeGetResources" $
         c'drmModeGetResources (reflect (Proxy ∷ Proxy drm))
  res ← cToResources =≪ peek ptr
  c'drmModeFreeResources ptr
  return res
