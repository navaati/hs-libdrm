{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

import FunctionalTools.Unicode

import System.DRM
import System.DRM.KMS.Utils
import System.DRM.KMS.Resources
import System.DRM.KMS.ModeInfo
import System.DRM.KMS.Crtc
import System.DRM.BufferObject.Dumb
import System.DRM.KMS.FrameBuffer

main ∷ IO ()
main = withDrm "/dev/dri/card0" $ \p → do
  (conn,_,crtc,mode):_ ← connectedResources =≪ getResources
  putStrLn "FBCon :"
  print (crtc `withSameTagAs` p)
  print =≪ getFb (crtcFbId crtc)
  let (w,h) = modeDisplay mode
  lf
  putStrLn "My buffer :"
  myBO ← let fI = fromIntegral in createDumbBO (fI w) (fI h)
  print (myBO `withSameTagAs` p)
  myFb ← getFb =≪ addFb myBO
  print myFb
  setCrtc (crtcId crtc) (fbId myFb) (0,0) [conn] mode
  pause
  setCrtc (crtcId crtc) (crtcFbId crtc) (crtcPosition crtc) [conn] mode
  rmFb $ fbId myFb
  boDestroy myBO

lf ∷ IO ()
lf = putStrLn ""

pause ∷ IO ()
pause = getLine ≫ return ()
