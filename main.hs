{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

import FunctionalTools.Unicode

import System.DRM
import System.DRM.KMS.Utils
import System.DRM.KMS.Resources
import System.DRM.KMS.ModeInfo
import System.DRM.KMS.Crtc
import System.DRM.BufferObject
import System.DRM.BufferObject.Dumb
import System.DRM.KMS.FrameBuffer

main ∷ IO ()
main = withDrm "/dev/dri/card0" $ \p → do
  (conn,_,crtc,mode):_ ← connectedResources =≪ getResources
  Just oldFb ← crtcFb (crtc `withSameTagAs` p)
  oldPosition ← crtcPosition crtc
  let (w,h) = modeDisplay mode
  
  myBO ← let fI = fromIntegral in createDumbBO (fI w) (fI h)
  myFb ← addFb myBO
  setCrtc crtc myFb (0,0) [conn] mode
  pause
  setCrtc crtc oldFb oldPosition [conn] mode
  rmFb myFb
  boDestroy myBO

pause ∷ IO ()
pause = getLine ≫ return ()
