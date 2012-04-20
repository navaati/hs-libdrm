{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Unicode

import System.DRM
import System.DRM.KMS.Utils
import System.DRM.KMS.Resources
import System.DRM.KMS.ModeInfo
import System.DRM.KMS.Crtc
import System.DRM.KMS.Connector
import System.DRM.BufferObject.Dumb
import System.DRM.FrameBuffer

main ∷ IO ()
main = do
  withDrm "/dev/dri/card0" $ \p → do
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
    setCrtc (crtcId crtc) (fbId myFb) (0,0) [(connectorId conn)] mode
    pause
    setCrtc (crtcId crtc) (crtcFbId crtc) (crtcPosition crtc) [(connectorId conn)] mode
    boDestroy myBO
  
  pause
  return ()

lf ∷ IO ()
lf = putStrLn ""

pause ∷ IO ()
pause = getLine ≫ return ()
