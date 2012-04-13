{-# LANGUAGE Rank2Types #-}

module Graphics.KMS.Utils where

import System.Posix
import Control.Exception(bracket)
import Data.Reflection
import Data.Proxy
import Control.Monad.Unicode
import Control.Applicative((<$>))

import Graphics.KMS.Types
import Graphics.KMS.Resources
import Graphics.KMS.Connector
import Graphics.KMS.Encoder
import Graphics.KMS.Crtc
import Graphics.KMS.ModeInfo

withDrm ∷ FilePath →
           (∀drm. (drm `Reifies` Drm) ⇒ Proxy drm → IO α) → IO α
withDrm drmPath f = bracket
                    (openFd drmPath ReadWrite Nothing defaultFileFlags)
                    closeFd
                    (\drm → reify (Drm drm) f)

connectedResources ∷ (drm `Reifies` Drm) ⇒
  Resources drm →
  IO [(Connector drm,Encoder drm,Crtc drm,ModeInfo)]
connectedResources res = do
  (filter isConnected <$> mapM getConnector (resConnectors res) ≫=) $
   mapM $ \conn → do
     enc ← getEncoder $ connectorCurrentEncoder conn
     crtc ← getCrtc $ encoderCrtcId enc
     return (conn,enc,crtc,crtcMode crtc)
