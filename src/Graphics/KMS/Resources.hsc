{-# LANGUAGE NoMonomorphismRestriction #-}

module Graphics.KMS.Resources (Resources(..), getResources) where

import Foreign
import Foreign.C
import System.Posix

#include<stdint.h>
#include<xf86drmMode.h>

import Graphics.KMS.Types
import Graphics.KMS.Utils

data Resources = Resources
                  { resFbs :: [FbId]
                  , resCrtcs :: [CrtcId]
                  , resConnectors :: [ConnectorId]
                  , resEncoders :: [EncoderId]
                  , resMinSize, resMaxSize :: (Word32, Word32)
                  } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeRes, field)

peekResources :: Ptr Resources -> IO Resources
peekResources ptr = do
  fbs <- pa (#p count_fbs) (#p fbs)
  crtcs <- pa (#p count_crtcs) (#p crtcs)
  connectors <- pa (#p count_connectors) (#p connectors)
  encoders <- pa (#p count_encoders) (#p encoders)
  [min_width, max_width, min_height, max_height] <-
    mapM ($ ptr) 
    [ (#p min_width)
    , (#p max_width)
    , (#p min_height)
    , (#p max_height)]
  return $ Resources fbs crtcs connectors encoders
    (min_width, min_height) (max_width, max_height)
  where pa = lPeekArray ptr

getResources :: (?drm :: Drm) ⇒ IO Resources
getResources = do
  ptr <- throwErrnoIfNull "drmModeGetResources" (drmModeGetResources ?drm)
  res <- peekResources ptr
  drmModeFreeResources ptr
  return res

foreign import ccall "drmModeGetResources"
  drmModeGetResources :: Drm -> IO (Ptr Resources)
foreign import ccall "drmModeFreeResources"
  drmModeFreeResources :: Ptr Resources -> IO ()
