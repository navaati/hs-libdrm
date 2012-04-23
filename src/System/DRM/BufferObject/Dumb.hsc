{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.DRM.BufferObject.Dumb (DumbBO, createDumbBO) where

import Foreign
import Foreign.C
import System.Posix
import Data.Reflection
import Data.Proxy

#include<xf86drm.h>
#include<drm_mode.h>
#include <sys/mman.h>

import System.DRM.Types
import System.DRM.BufferObject

data DumbBO drm = DumbBO
                  (BOHandle drm)
                  (Width,Height) Pitch Size
                  (Ptr ()) deriving Show

createDumbBO ∷ ∀drm. (RDrm drm) ⇒
  Width → Height → IO (DumbBO drm)
createDumbBO w h =
  allocaBytes (#size struct drm_mode_create_dumb) $ \p → do
    (#poke struct drm_mode_create_dumb, width) p w
    (#poke struct drm_mode_create_dumb, height) p h
    (#poke struct drm_mode_create_dumb, bpp) p bpp
    (#poke struct drm_mode_create_dumb, flags) p (0 ∷ Word32)
    throwErrnoIfMinus1_ "DRM_IOCTL_MODE_CREATE_DUMB" $
      drmIoctl (reflect (Proxy ∷ Proxy drm))
      (#const DRM_IOCTL_MODE_CREATE_DUMB) p
    handle ← (#peek struct drm_mode_create_dumb, handle) p
    size ← (#peek struct drm_mode_create_dumb, size) p
    pitch ← (#peek struct drm_mode_create_dumb, pitch) p
    mapPtr ← allocaBytes (#size struct drm_mode_map_dumb) $ \p' → do
      (#poke struct drm_mode_map_dumb, handle) p' handle
      throwErrnoIfMinus1_ "DRM_IOCTL_MODE_MAP_DUMB" $
        drmIoctl (reflect handle)
        (#const DRM_IOCTL_MODE_MAP_DUMB) p'
      offset ← (#peek struct drm_mode_map_dumb, offset) p'
      throwErrnoIfMinus1 "mmap DumbBO" $ fmap ptrToIntPtr $
        dumb_mmap (fromIntegral size) (reflect handle) offset
    return $ DumbBO handle (w,h) pitch size (intPtrToPtr mapPtr)

instance (RDrm drm) ⇒ BufferObject (DumbBO drm) drm where
  boHandle (DumbBO handle _ _ _ _) = handle
  boSize (DumbBO _ _ _ size _) = size
  boDestroy (DumbBO handle _ _ size mapPtr) = do
    munmap mapPtr (fromIntegral size)
    allocaBytes (#size struct drm_mode_destroy_dumb) $ \p → do
      (#poke struct drm_mode_destroy_dumb, handle) p handle
      throwErrnoIfMinus1_ "DRM_IOCTL_MODE_DESTROY_DUMB" $
        drmIoctl (reflect handle)
        (#const DRM_IOCTL_MODE_DESTROY_DUMB) p

instance (RDrm drm) ⇒ ImageBO (DumbBO drm) drm where
  boRes (DumbBO _ res _ _ _) = res
  boPitch (DumbBO _ _ pitch _ _) = pitch
  boBPP _ = fromIntegral bpp
  boDepth _ = 24

bpp ∷ Word32
bpp = 32

instance (RDrm drm) ⇒ MappableBO (DumbBO drm) drm where
  boMap (DumbBO _ _ _ _ mapPtr) = return $ castPtr mapPtr


foreign import ccall "drmIoctl"
  drmIoctl ∷ Drm → CULong → Ptr () → IO CInt
foreign import ccall "dumb_mmap"
  dumb_mmap ∷ CSize → Drm → Word64 → IO (Ptr a)
foreign import ccall "munmap"
  munmap ∷ Ptr a → CSize → IO ()
