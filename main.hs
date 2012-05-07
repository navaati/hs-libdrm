{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

import FunctionalTools.Unicode
import Data.Maybe(fromJust)

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
  oldFb ← (getFb ∘ fromJust) =≪ crtcFb (crtc `withSameTagAs` p)
  print oldFb
  oldPosition ← crtcPosition crtc
  let (w,h) = modeDisplay mode
  lf
  putStrLn "My buffer :"
  myBO ← let fI = fromIntegral in createDumbBO (fI w) (fI h)
  print myBO
  myFb ← getFb =≪ addFb myBO
  print myFb
  setCrtc crtc (fbId myFb) (0,0) [conn] mode
  pause
  setCrtc crtc (fbId oldFb) oldPosition [conn] mode
  rmFb $ fbId myFb
  boDestroy myBO

lf ∷ IO ()
lf = putStrLn ""

pause ∷ IO ()
pause = getLine ≫ return ()
