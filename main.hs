{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}

import Control.Monad.Unicode

import System.DRM
import System.DRM.KMS.Utils
import System.DRM.KMS.Resources
import System.DRM.KMS.Connector

main ∷ IO ()
main = do
  withDrm "/dev/dri/card0" $ \p → do
    putStrLn "Current ressources :"
    resIds ← getResources
    lf ≫ print (resIds `withSameTagAs` p)
    (connectedResources resIds ≫=) $ mapM_ $ \(conn,enc,crtc,_) → do
      lf ≫ lf
      print $ conn { connectorModeInfo = []}
      lf ≫ print enc
      lf ≫ print crtc
  
  _ ← getLine
  return ()

lf ∷ IO ()
lf = putStrLn ""
