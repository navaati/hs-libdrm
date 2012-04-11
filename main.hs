{-# LANGUAGE UnicodeSyntax, FlexibleContexts, ScopedTypeVariables, TypeOperators #-}

import Prelude.Unicode
import Control.Monad.Unicode
import Data.Reflection
import Data.Proxy
--import Data.Bits

import Graphics.KMS.Types
import Graphics.KMS.Utils
import Graphics.KMS.Resources
import Graphics.KMS.Connector
import Graphics.KMS.ModeInfo
import Graphics.KMS.Encoder
import Graphics.KMS.Crtc

main ∷ IO ()
main = withDrm "/dev/dri/card0" $ \(Proxy ∷ Proxy drm) → do
  (conn,enc,crtc ∷ Crtc drm,mode) ← currentResources
  
  print $ conn { connectorModeInfo = []}
  lf ≫ print enc
  lf ≫ print crtc
  lf ≫ print mode
  
  _ ← getLine
  return ()

currentResources ∷ (drm `Reifies` Drm) ⇒
              IO (Connector drm,Encoder drm,Crtc drm,ModeInfo)
currentResources = do
  conn ← (fmap (head ∘ filter isConnected) ∘ mapM getConnector ∘ resConnectors)
    =≪ getResources
  enc ← getEncoder $ connectorCurrentEncoder conn
  crtc ← getCrtc $ encoderCrtcId enc
  return (conn,enc,crtc,crtcMode crtc)

{-printMode ∷ ModeInfo → IO ()
printMode mode = do
  lf
  print mode
  putStr "Flags :\t"
  print . showBits $ modeFlags mode
  putStr "Type :\t"
  print . showBits $ modeType mode
  where
    showBits ∷ (Bits n, Bounded n) ⇒ n → String
    showBits x = foldr (\b → (:) $ if testBit x b then '1' else '0' ) "" $ reverse [0..(bitSize x)-1]-}

lf ∷ IO ()
lf = putStrLn ""
