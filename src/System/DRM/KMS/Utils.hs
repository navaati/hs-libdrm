module System.DRM.KMS.Utils where

import Control.Monad.Unicode
import Control.Applicative((<$>))

import System.DRM.Types
import System.DRM.KMS.Resources
import System.DRM.KMS.Connector
import System.DRM.KMS.Encoder
import System.DRM.KMS.Crtc
import System.DRM.KMS.ModeInfo

connectedResources ∷ (RDrm drm) ⇒
  Resources drm →
  IO [(Connector drm,Encoder drm,Crtc drm,ModeInfo)]
connectedResources res = do
  (filter isConnected <$> mapM getConnector (resConnectors res) ≫=) $
   mapM $ \conn → do
     enc ← getEncoder $ connectorCurrentEncoder conn
     crtc ← getCrtc $ encoderCrtcId enc
     return (conn,enc,crtc,crtcMode crtc)
