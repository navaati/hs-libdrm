module System.DRM.KMS.ModeInfo where

import Foreign
import Foreign.C.String
import System.DRM.FFIUtils

#include<stdint.h>
#include<xf86drmMode.h>

data ModeInfo = ModeInfo
                { modeClock ∷ Word32
                , modeDisplay, modeTotal ∷ (Word16, Word16)
                , modeSync ∷ ((Word16, Word16),(Word16, Word16))
                , modeHSkew, modeVScan ∷ Word16
                , modeVRefresh ∷ Word32
                , modeFlags ∷ ModeFlags
                , modeType ∷ ModeType
                , modeName ∷ String
                } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeModeInfo, field)
#define hsc_f(field) hsc_ptr(drmModeModeInfo, field)

instance Storable ModeInfo where
  sizeOf _ = #size drmModeModeInfo
  alignment _ = undefined
  peek ptr = do
    [hDisplay, vDisplay, hSyncStart, vSyncStart, hSyncEnd,
     vSyncEnd, hTotal, vTotal, hSkew, vScan] ←
      mapM ($ ptr)
      [ (#p hdisplay), (#p vdisplay)
      , (#p hsync_start), (#p vsync_start)
      , (#p hsync_end), (#p vsync_end)
      , (#p htotal), (#p vtotal)
      , (#p hskew), (#p vscan)]
    [clock, vRefresh] ←
      mapM ($ ptr)
      [ (#p clock)
      , (#p vrefresh)]
    flags ← peekFlags modeFlagEnum $ (#f flags) ptr
    mType ← peekFlags typeFlagEnum $ (#f type) ptr
    name ← peekCAString $ (#ptr drmModeModeInfo, name) ptr
    return $ ModeInfo
      clock
      (hDisplay, vDisplay)
      (hTotal, vTotal)
      ((hSyncStart, vSyncStart),(hSyncEnd, vSyncEnd))
      hSkew vScan
      vRefresh flags mType name
  poke _ _ = return ()

type ModeFlags = [ModeFlag]
data ModeFlag = PHSync | NHSync | PVSync | NVSync | Interlace | DBLScan | CSync | PCSync | NCSync | HSkew | BCast | PixΜx | DBLClk | ClkDiv2 deriving (Show, Eq)

modeFlagEnum ∷ [(ModeFlag,Word32)]
modeFlagEnum = [
  (PHSync,(#const DRM_MODE_FLAG_PHSYNC)),
  (NHSync,(#const DRM_MODE_FLAG_NHSYNC)),
  (PVSync,(#const DRM_MODE_FLAG_PVSYNC)),
  (NVSync,(#const DRM_MODE_FLAG_NVSYNC)),
  (Interlace,(#const DRM_MODE_FLAG_INTERLACE)),
  (DBLScan,(#const DRM_MODE_FLAG_DBLSCAN)),
  (CSync,(#const DRM_MODE_FLAG_CSYNC)),
  (PCSync,(#const DRM_MODE_FLAG_PCSYNC)),
  (NCSync,(#const DRM_MODE_FLAG_NCSYNC)),
  (HSkew,(#const DRM_MODE_FLAG_HSKEW)),
  (BCast,(#const DRM_MODE_FLAG_BCAST)),
  (PixΜx,(#const DRM_MODE_FLAG_PIXMUX)),
  (DBLClk,(#const DRM_MODE_FLAG_DBLCLK)),
  (ClkDiv2,(#const DRM_MODE_FLAG_CLKDIV2))]

type ModeType = [ModeTypeFlag]
data ModeTypeFlag = Builtin | ClockC | CrtcC | Preferred | Default | UserDef | Driver deriving (Show, Eq)

typeFlagEnum ∷ [(ModeTypeFlag,Word32)]
typeFlagEnum = [
  (Builtin,(#const DRM_MODE_TYPE_BUILTIN)),
  (ClockC,(#const DRM_MODE_TYPE_CLOCK_C)),
  (CrtcC,(#const DRM_MODE_TYPE_CRTC_C)),
  (Preferred,(#const DRM_MODE_TYPE_PREFERRED)),
  (Default,(#const DRM_MODE_TYPE_DEFAULT)),
  (UserDef,(#const DRM_MODE_TYPE_USERDEF)),
  (Driver,(#const DRM_MODE_TYPE_DRIVER))]
