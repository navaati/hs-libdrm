{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module System.DRM.KMS.Utils where

import FunctionalTools.Unicode

import System.DRM.Types
import System.DRM.KMS.Resources
import System.DRM.KMS.Connector
import System.DRM.KMS.Encoder
import System.DRM.KMS.Crtc
import System.DRM.KMS.ModeInfo

connectedResources ∷ (RDrm drm) ⇒
  Resources drm →
  IO [(Connector drm,Encoder drm,Crtc drm,ModeInfo)]
connectedResources = (resConnectors ⋙ filterM (isConnected)) >=>
  mapM (\conn → do
           enc ← connectorCurrentEncoder conn
           crtc ← encoderCrtc enc
           Just mode ← crtcMode crtc
           return (conn,enc,crtc,mode))
