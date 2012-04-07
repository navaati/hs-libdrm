module Graphics.KMS.ModeInfo where

import Foreign
import Foreign.C.String

#include<stdint.h>
#include<xf86drmMode.h>

data ModeInfo = ModeInfo
                { modeClock :: Word32
                , modeDisplay, modeTotal :: (Word16, Word16)
                , modeSync :: ((Word16, Word16),(Word16, Word16))
                , modeHSkew, modeVScan :: Word16
                , modeVRefresh :: Word32
                , modeFlags :: ModeFlags
                , modeType :: ModeType
                , modeName :: String
                } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeModeInfo, field)

instance Storable ModeInfo where
  sizeOf _ = #size drmModeModeInfo
  alignment _ = undefined
  peek ptr = do
    [hDisplay, vDisplay, hSyncStart, vSyncStart, hSyncEnd,
     vSyncEnd, hTotal, vTotal, hSkew, vScan] <- 
      mapM ($ ptr)
      [ (#p hdisplay), (#p vdisplay)
      , (#p hsync_start), (#p vsync_start)
      , (#p hsync_end), (#p vsync_end)
      , (#p htotal), (#p vtotal)
      , (#p hskew), (#p vscan)]
    [clock, vRefresh, flags, mType] <-
      mapM ($ ptr) 
      [ (#p clock)
      , (#p vrefresh)
      , (#p flags)
      , (#p type)]
    name <- peekCAString $ (#ptr drmModeModeInfo, name) ptr
    return $ ModeInfo 
      clock
      (hDisplay, vDisplay)
      (hTotal, vTotal)
      ((hSyncStart, vSyncStart),(hSyncEnd, vSyncEnd))
      hSkew vScan
      vRefresh flags mType name
  poke _ _ = return ()

type ModeFlags = Word32
type ModeType = Word32
