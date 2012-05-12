{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

#include <bindings.dsl.h>

#include<stdint.h>
#include<xf86drmMode.h>

module System.DRM.C.KMS.ModeInfo (C'drmModeModeInfo,cToModeInfo,modeInfoToC) where
#strict_import
import FunctionalTools.Unicode
import Foreign.C.String(castCCharToChar,castCharToCChar)
import System.DRM.FFIUtils

import System.DRM.KMS.ModeInfo

#starttype drmModeModeInfo
#field clock, Word32
#field hdisplay, Word16
#field vdisplay, Word16
#field hsync_start, Word16
#field vsync_start, Word16
#field hsync_end, Word16
#field vsync_end, Word16
#field htotal, Word16
#field vtotal, Word16
#field hskew, Word16
#field vscan, Word16
#field vrefresh, Word32
#field flags, Word32
#field type, Word32
#array_field name, CChar
#stoptype

#num DRM_MODE_FLAG_PHSYNC
#num DRM_MODE_FLAG_NHSYNC
#num DRM_MODE_FLAG_PVSYNC
#num DRM_MODE_FLAG_NVSYNC
#num DRM_MODE_FLAG_INTERLACE
#num DRM_MODE_FLAG_DBLSCAN
#num DRM_MODE_FLAG_CSYNC
#num DRM_MODE_FLAG_PCSYNC
#num DRM_MODE_FLAG_NCSYNC
#num DRM_MODE_FLAG_HSKEW
#num DRM_MODE_FLAG_BCAST
#num DRM_MODE_FLAG_PIXMUX
#num DRM_MODE_FLAG_DBLCLK
#num DRM_MODE_FLAG_CLKDIV2

#num DRM_MODE_TYPE_BUILTIN
#num DRM_MODE_TYPE_CLOCK_C
#num DRM_MODE_TYPE_CRTC_C
#num DRM_MODE_TYPE_PREFERRED
#num DRM_MODE_TYPE_DEFAULT
#num DRM_MODE_TYPE_USERDEF
#num DRM_MODE_TYPE_DRIVER

cToModeInfo ∷ C'drmModeModeInfo → ModeInfo
cToModeInfo (C'drmModeModeInfo {..}) =
  ModeInfo
  c'drmModeModeInfo'clock
  (c'drmModeModeInfo'hdisplay, c'drmModeModeInfo'vdisplay)
  (c'drmModeModeInfo'htotal, c'drmModeModeInfo'vtotal)
  ((c'drmModeModeInfo'hsync_start, c'drmModeModeInfo'vsync_start),
   (c'drmModeModeInfo'hsync_end, c'drmModeModeInfo'vsync_end))
  c'drmModeModeInfo'hskew c'drmModeModeInfo'vscan
  c'drmModeModeInfo'vrefresh 
  (cToFlags modeFlagEnum c'drmModeModeInfo'flags)
  (cToFlags typeFlagEnum c'drmModeModeInfo'type)
  (map castCCharToChar $ takeWhile (≢ 0) c'drmModeModeInfo'name)

modeInfoToC ∷ ModeInfo → C'drmModeModeInfo
modeInfoToC
  (ModeInfo
   c'drmModeModeInfo'clock
   (c'drmModeModeInfo'hdisplay,c'drmModeModeInfo'vdisplay)
   (c'drmModeModeInfo'htotal,c'drmModeModeInfo'vtotal)
   ((c'drmModeModeInfo'hsync_start, c'drmModeModeInfo'vsync_start),
    (c'drmModeModeInfo'hsync_end, c'drmModeModeInfo'vsync_end))
   c'drmModeModeInfo'hskew c'drmModeModeInfo'vscan
   c'drmModeModeInfo'vrefresh
   flags
   types
   name)
  = let c'drmModeModeInfo'flags = flagsToC modeFlagEnum flags
        c'drmModeModeInfo'type = flagsToC typeFlagEnum types
        c'drmModeModeInfo'name = (map castCharToCChar name) ⧺ [0]
    in C'drmModeModeInfo{..}


modeFlagEnum ∷ [(ModeFlag,Word32)]
modeFlagEnum = [
  (PHSync,c'DRM_MODE_FLAG_PHSYNC),
  (NHSync,c'DRM_MODE_FLAG_NHSYNC),
  (PVSync,c'DRM_MODE_FLAG_PVSYNC),
  (NVSync,c'DRM_MODE_FLAG_NVSYNC),
  (Interlace,c'DRM_MODE_FLAG_INTERLACE),
  (DBLScan,c'DRM_MODE_FLAG_DBLSCAN),
  (CSync,c'DRM_MODE_FLAG_CSYNC),
  (PCSync,c'DRM_MODE_FLAG_PCSYNC),
  (NCSync,c'DRM_MODE_FLAG_NCSYNC),
  (HSkew,c'DRM_MODE_FLAG_HSKEW),
  (BCast,c'DRM_MODE_FLAG_BCAST),
  (PixMux,c'DRM_MODE_FLAG_PIXMUX),
  (DBLClk,c'DRM_MODE_FLAG_DBLCLK),
  (ClkDiv2,c'DRM_MODE_FLAG_CLKDIV2)]

typeFlagEnum ∷ [(ModeTypeFlag,Word32)]
typeFlagEnum = [
  (Builtin,c'DRM_MODE_TYPE_BUILTIN),
  (ClockC,c'DRM_MODE_TYPE_CLOCK_C),
  (CrtcC,c'DRM_MODE_TYPE_CRTC_C),
  (Preferred,c'DRM_MODE_TYPE_PREFERRED),
  (Default,c'DRM_MODE_TYPE_DEFAULT),
  (UserDef,c'DRM_MODE_TYPE_USERDEF),
  (Driver,c'DRM_MODE_TYPE_DRIVER)]
