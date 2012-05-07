{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include<stdint.h>
#include<xf86drmMode.h>

module System.DRM.C.KMS.Encoder where
#strict_import
import System.DRM.Types
import System.Posix.Types

#starttype drmModeEncoder
#field encoder_id, Word32
#field encoder_type, Word32
#field crtc_id, Word32
#field possible_crtcs, Word32
#field possible_clones, Word32
#stoptype

#num DRM_MODE_ENCODER_NONE
#num DRM_MODE_ENCODER_DAC
#num DRM_MODE_ENCODER_TMDS
#num DRM_MODE_ENCODER_LVDS
#num DRM_MODE_ENCODER_TVDAC

#ccall drmModeGetEncoder, Drm → EncoderId drm → IO (Ptr <drmModeEncoder>)
#ccall drmModeFreeEncoder, Ptr <drmModeEncoder> → IO ()
