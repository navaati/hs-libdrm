{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include<stdint.h>
#include<xf86drmMode.h>

module System.DRM.C.KMS.FrameBuffer where
#strict_import
import System.DRM.Types
import System.Posix.Types

#starttype drmModeFB
#field fb_id, Word32
#field width, Word32
#field height, Word32
#field pitch, Word32
#field bpp, Word32
#field depth, Word32
#field handle, Word32
#stoptype

#ccall drmModeGetFB, Drm → Fb drm → IO (Ptr <drmModeFB>)
#ccall drmModeFreeFB, Ptr <drmModeFB> → IO ()
#ccall drmModeAddFB, Drm → Width → Height → Depth → BPP → Pitch → BOHandle drm → Ptr (Fb drm) → IO CInt
#ccall drmModeRmFB, Drm → Fb drm → IO CInt
