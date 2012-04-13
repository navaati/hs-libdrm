{-# LANGUAGE UnicodeSyntax, FlexibleContexts, ScopedTypeVariables #-}

import Control.Monad.Unicode
import Data.Proxy

import Graphics.KMS.Utils
import Graphics.KMS.Resources
import Graphics.KMS.Connector

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
