{-# LANGUAGE UnicodeSyntax, FlexibleContexts, ScopedTypeVariables #-}

import Control.Monad.Unicode
import Data.Proxy

import System.DRM
import System.DRM.KMS.Utils
import System.DRM.KMS.Resources
import System.DRM.KMS.Connector

main ∷ IO ()
main = do
  withDrm "/dev/dri/card0" $ \(Proxy ∷ Proxy drm) → do
    putStrLn "Current ressources :"
    (resIds ∷ Resources drm) ← getResources
    lf ≫ print resIds
    (connectedResources resIds ≫=) $ mapM_ $ \(conn,enc,crtc,_) → do
      lf ≫ lf
      print $ conn { connectorModeInfo = []}
      lf ≫ print enc
      lf ≫ print crtc
  
  _ ← getLine
  return ()

lf ∷ IO ()
lf = putStrLn ""
