{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Unicode

import System.DRM
import System.DRM.KMS.Utils
import System.DRM.KMS.Resources
import System.DRM.KMS.ModeInfo
import System.DRM.KMS.Connector
import System.DRM.BufferObject.Dumb

main ∷ IO ()
main = do
  withDrm "/dev/dri/card0" $ \p → do
    (_,_,crtc,oldMode):_ ← connectedResources =≪ getResources
    print (crtc `withSameTagAs` p)
    let (w,h) = modeDisplay oldMode
    myBO ← let fI = fromIntegral in createDumbBO (fI w) (fI h)
    lf
    print (myBO `withSameTagAs` p)
    boDestroy myBO
  
  _ ← getLine
  return ()

lf ∷ IO ()
lf = putStrLn ""
