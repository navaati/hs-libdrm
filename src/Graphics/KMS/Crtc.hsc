module Graphics.KMS.Crtc where

import Foreign
import Foreign.C
import System.Posix

#include<stdint.h>
#include<xf86drmMode.h>

import Graphics.KMS.ModeInfo
import Graphics.KMS.Types

data Crtc = ConnectedCrtc
            { crtcId :: CrtcId
            , crtcFbId :: FbId
            , crtcPosition :: (Word32,Word32)
            , crtcPxSize :: (Word32,Word32)
            , crtcMode :: ModeInfo
            , crtcGammaSize :: Int
            } |
            DisconnectedCrtc
            { crtcId :: CrtcId
            , crtcPosition :: (Word32,Word32)
            , crtcPxSize :: (Word32,Word32)
            , crtcGammaSize :: Int
            } deriving (Show)
              
#define hsc_p(field) hsc_peek(drmModeCrtc, field)

peekCrtc :: Ptr Crtc -> IO Crtc
peekCrtc ptr = do
  cId <- (#p crtc_id) ptr
  fbId <- (#p buffer_id) ptr
  [x, y, w, h] <-
    mapM ($ ptr)
    [ (#p x), (#p y)
    , (#p width), (#p height)]
  gammaSize <- (#p gamma_size) ptr
  if fbId == 0
    then return $ DisconnectedCrtc cId (x,y) (w,h) gammaSize
    else do
    modeValid <- fmap toBool ((#p mode_valid) ptr :: IO CInt)
    if modeValid then return () else error "Connected CRTC mode not valid"
    mode <- (#p mode) ptr
    return $ ConnectedCrtc cId (FbId fbId) (x,y) (w,h) mode gammaSize

getCrtc :: (?drm :: Drm) ⇒ CrtcId -> IO Crtc
getCrtc cId = do
  ptr <- throwErrnoIfNull "drmModeGetCrtc" (drmModeGetCrtc ?drm cId)
  crtc <- peekCrtc ptr
  drmModeFreeCrtc ptr
  return crtc

foreign import ccall "drmModeGetCrtc"
  drmModeGetCrtc :: Drm -> CrtcId -> IO (Ptr Crtc)
foreign import ccall "drmModeFreeCrtc"
  drmModeFreeCrtc :: Ptr Crtc -> IO ()
