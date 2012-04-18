{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.DRM.KMS.Resources (Resources(..), getResources) where

import Foreign
import Foreign.C
import System.Posix
import Data.Reflection
import Data.Proxy

#include<stdint.h>
#include<xf86drmMode.h>

import System.DRM.Types
import System.DRM.FFIUtils

data Resources drm = Resources
                     { resFbs ∷ [FbId drm]
                     , resCrtcs ∷ [CrtcId drm]
                     , resConnectors ∷ [ConnectorId drm]
                     , resEncoders ∷ [EncoderId drm]
                     , resMinSize, resMaxSize ∷ (Word32, Word32)
                     } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeRes, field)

peekResources ∷ ResourcesPtr drm → IO (Resources drm)
peekResources ptr = do
  fbs ← pa (#p count_fbs) (#p fbs)
  crtcs ← pa (#p count_crtcs) (#p crtcs)
  connectors ← pa (#p count_connectors) (#p connectors)
  encoders ← pa (#p count_encoders) (#p encoders)
  [min_width, max_width, min_height, max_height] ←
    mapM ($ ptr)
    [ (#p min_width)
    , (#p max_width)
    , (#p min_height)
    , (#p max_height)]
  return $ Resources fbs crtcs connectors encoders
    (min_width, min_height) (max_width, max_height)
  where pa = lPeekArray ptr

getResources ∷ ∀drm. (RDrm drm) ⇒
                IO (Resources drm)
getResources = do
  ptr ← throwErrnoIfNull "drmModeGetResources" $
         drmModeGetResources (reflect (Proxy ∷ Proxy drm))
  res ← peekResources ptr
  drmModeFreeResources ptr
  return res

type ResourcesPtr drm = Ptr (Resources drm)

foreign import ccall "drmModeGetResources"
  drmModeGetResources ∷ Drm → IO (ResourcesPtr drm)
foreign import ccall "drmModeFreeResources"
  drmModeFreeResources ∷ ResourcesPtr drm → IO ()
