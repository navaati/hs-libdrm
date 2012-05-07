{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include<stdint.h>
#include<xf86drmMode.h>

module System.DRM.C.KMS.Crtc where
#strict_import
import System.DRM.C.KMS.ModeInfo
import System.DRM.Types
import System.Posix.Types

#starttype drmModeCrtc
#field crtc_id, Word32
#field buffer_id, Word32
#field x, Word32
#field y, Word32
#field width, Word32
#field height, Word32
#field mode_valid, CInt
#field mode, <drmModeModeInfo>
#field gamma_size, CInt
#stoptype

#ccall drmModeGetCrtc, Drm → CrtcId drm → IO (Ptr <drmModeCrtc>)
#ccall drmModeFreeCrtc, Ptr <drmModeCrtc> → IO ()
#ccall drmModeSetCrtc, Drm → CrtcId drm → FbId drm → Word32 → Word32 → Ptr (ConnectorId drm) → CInt → Ptr <drmModeModeInfo> → IO CInt
