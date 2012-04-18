{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include<stdint.h>
#include<xf86drmMode.h>

module System.DRM.C.KMS.Resources where
#strict_import
import System.DRM.Types
import System.Posix.Types

#starttype drmModeRes
#field count_fbs, CInt
#field fbs, Ptr Word32
#field count_crtcs, CInt
#field crtcs, Ptr Word32
#field count_connectors, CInt
#field connectors, Ptr Word32
#field count_encoders, CInt
#field encoders, Ptr Word32
#field min_width, Word32
#field min_height, Word32
#field max_width, Word32
#field max_height, Word32
#stoptype

#ccall drmModeGetResources, Drm → IO (Ptr <drmModeRes>)
#ccall drmModeFreeResources, Ptr <drmModeRes> → IO ()
