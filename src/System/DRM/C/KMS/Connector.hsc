{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include<stdint.h>
#include<xf86drmMode.h>

module System.DRM.C.KMS.Connector where
#strict_import
import System.DRM.C.KMS.ModeInfo
import System.DRM.Types
import System.Posix.Types

#starttype drmModeConnector
#field connector_id, Word32
#field encoder_id, Word32
#field connector_type, Word32
#field connector_type_id, Word32
#field connection, <drmModeConnection>
#field mmWidth, Word32
#field mmHeight, Word32
#field subpixel, <drmModeSubPixel>
#field count_modes, CInt
#field modes, Ptr <drmModeModeInfo>
#field count_props, CInt
#field props, Ptr Word32
#field prop_values, Ptr Word64
#field count_encoders, CInt
#field encoders, Ptr Word32
#stoptype

#integral_t drmModeConnection
#num DRM_MODE_CONNECTED
#num DRM_MODE_DISCONNECTED  
#num DRM_MODE_UNKNOWNCONNECTION

#integral_t drmModeSubPixel
#num DRM_MODE_SUBPIXEL_UNKNOWN
#num DRM_MODE_SUBPIXEL_HORIZONTAL_RGB
#num DRM_MODE_SUBPIXEL_HORIZONTAL_BGR
#num DRM_MODE_SUBPIXEL_VERTICAL_RGB
#num DRM_MODE_SUBPIXEL_VERTICAL_BGR
#num DRM_MODE_SUBPIXEL_NONE

#num DRM_MODE_CONNECTOR_Unknown
#num DRM_MODE_CONNECTOR_VGA
#num DRM_MODE_CONNECTOR_DVII
#num DRM_MODE_CONNECTOR_DVID
#num DRM_MODE_CONNECTOR_DVIA
#num DRM_MODE_CONNECTOR_Composite
#num DRM_MODE_CONNECTOR_SVIDEO
#num DRM_MODE_CONNECTOR_LVDS
#num DRM_MODE_CONNECTOR_Component
#num DRM_MODE_CONNECTOR_9PinDIN
#num DRM_MODE_CONNECTOR_DisplayPort
#num DRM_MODE_CONNECTOR_HDMIA
#num DRM_MODE_CONNECTOR_HDMIB
#num DRM_MODE_CONNECTOR_TV
#num DRM_MODE_CONNECTOR_eDP

#ccall drmModeGetConnector, Drm → Connector drm → IO (Ptr <drmModeConnector>)
#ccall drmModeFreeConnector, Ptr <drmModeConnector> → IO ()
