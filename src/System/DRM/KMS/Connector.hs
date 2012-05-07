{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module System.DRM.KMS.Connector
       ( connectorCurrentEncoder
       , connectorType
       , connectorTypeId
       , connectorConnection
       , connectorSize
       , connectorSubpixel
       , connectorModes
       , connectorProperties
       , connectorEncoders
       , Connection(..)
       , isConnected
       , SubPixel(..)
       , ConnectorType(..)
       , Property(..)
       ) where

import FunctionalTools.Unicode

import Foreign
import Foreign.C
import Data.Maybe(fromJust)

import System.DRM.C.KMS.Connector
import System.DRM.Types
import System.DRM.KMS.ModeInfo
import System.DRM.C.KMS.ModeInfo

connectorCurrentEncoder ∷ (RDrm drm) ⇒ Connector drm → IO (EncoderId drm)
connectorCurrentEncoder = fmap (EncoderId ∘ c'drmModeConnector'encoder_id) ∘ getConnector

connectorType ∷ (RDrm drm) ⇒ Connector drm → IO ConnectorType
connectorType =
  fmap (fromJust ∘ flip lookup connectorTypeEnum ∘ c'drmModeConnector'connector_type)
  ∘ getConnector

connectorTypeId ∷ (RDrm drm) ⇒ Connector drm → IO Word32
connectorTypeId = fmap c'drmModeConnector'connector_type_id ∘ getConnector

connectorConnection ∷ (RDrm drm) ⇒ Connector drm → IO Connection
connectorConnection =
  fmap (fromJust ∘ flip lookup connectionEnum ∘ c'drmModeConnector'connection)
  ∘ getConnector

connectorSize ∷ (RDrm drm) ⇒ Connector drm → IO (Width,Height)
connectorSize = fmap (c'drmModeConnector'mmWidth &&& c'drmModeConnector'mmHeight)
                ∘ getConnector

connectorSubpixel ∷ (RDrm drm) ⇒ Connector drm → IO SubPixel
connectorSubpixel =
  fmap (fromJust ∘ flip lookup subpixelEnum ∘ c'drmModeConnector'subpixel)
  ∘ getConnector

connectorModes ∷ (RDrm drm) ⇒ Connector drm → IO [ModeInfo]
connectorModes =
  fmap (fmap cToModeInfo) ∘ liftM2 peekArray
  (fromIntegral ∘ c'drmModeConnector'count_modes) c'drmModeConnector'modes
  <=< getConnector

connectorProperties ∷ (RDrm drm) ⇒ Connector drm → IO [Property]
connectorProperties =
  liftM2 (liftM2 $ zipWith (const ∘ const ()))
  (liftM2 peekArray
   (fromIntegral ∘ c'drmModeConnector'count_props) c'drmModeConnector'props)
  (liftM2 peekArray
   (fromIntegral ∘ c'drmModeConnector'count_props) c'drmModeConnector'prop_values)
  <=< getConnector

connectorEncoders ∷ (RDrm drm) ⇒ Connector drm → IO [EncoderId drm]
connectorEncoders =
  fmap (fmap EncoderId) ∘ liftM2 peekArray
  (fromIntegral ∘ c'drmModeConnector'count_encoders) c'drmModeConnector'encoders
  <=< getConnector

getConnector ∷ (RDrm drm) ⇒
  Connector drm → IO C'drmModeConnector
getConnector cId = do
  ptr ← throwErrnoIfNull "drmModeGetConnector" $
    applyDrm c'drmModeGetConnector cId
  connector ← peek ptr
  c'drmModeFreeConnector ptr
  return connector

data Connection = Connected | Disconnected | UnknownConnection deriving (Show, Eq)

connectionEnum ∷ [(C'drmModeConnection,Connection)]
connectionEnum = [
  (c'DRM_MODE_CONNECTED, Connected) ,
  (c'DRM_MODE_DISCONNECTED, Disconnected) ,
  (c'DRM_MODE_UNKNOWNCONNECTION, UnknownConnection) ]

isConnected ∷ RDrm drm ⇒ Connector drm → IO Bool
isConnected = fmap (≡Connected) ∘ connectorConnection

data SubPixel = UnknownSubPixel | HorizontalRGB | HorizontalBGR | VerticalRGB | VerticalBGR | None deriving (Show, Eq)

subpixelEnum ∷ [(C'drmModeSubPixel,SubPixel)]
subpixelEnum = [
  (c'DRM_MODE_SUBPIXEL_UNKNOWN, UnknownSubPixel) ,
  (c'DRM_MODE_SUBPIXEL_HORIZONTAL_RGB, HorizontalRGB) ,
  (c'DRM_MODE_SUBPIXEL_HORIZONTAL_BGR, HorizontalBGR) ,
  (c'DRM_MODE_SUBPIXEL_VERTICAL_RGB, VerticalRGB) ,
  (c'DRM_MODE_SUBPIXEL_VERTICAL_BGR, VerticalBGR) ,
  (c'DRM_MODE_SUBPIXEL_NONE, None) ]

data ConnectorType = UnknownConnectorType | VGA | DVII | DVID | DVIA | Composite | SVIDEO | LVDS | Component | NinePinDIN | DisplayPort | HDMIA | HDMIB | TV | EDP deriving (Show, Eq)

connectorTypeEnum ∷ [(Word32,ConnectorType)]
connectorTypeEnum = [
  (c'DRM_MODE_CONNECTOR_Unknown, UnknownConnectorType) ,
  (c'DRM_MODE_CONNECTOR_VGA, VGA) ,
  (c'DRM_MODE_CONNECTOR_DVII, DVII) ,
  (c'DRM_MODE_CONNECTOR_DVID, DVID) ,
  (c'DRM_MODE_CONNECTOR_DVIA, DVIA) ,
  (c'DRM_MODE_CONNECTOR_Composite, Composite) ,
  (c'DRM_MODE_CONNECTOR_SVIDEO, SVIDEO) ,
  (c'DRM_MODE_CONNECTOR_LVDS, LVDS) ,
  (c'DRM_MODE_CONNECTOR_Component, Component) ,
  (c'DRM_MODE_CONNECTOR_9PinDIN, NinePinDIN) ,
  (c'DRM_MODE_CONNECTOR_DisplayPort, DisplayPort) ,
  (c'DRM_MODE_CONNECTOR_HDMIA, HDMIA) ,
  (c'DRM_MODE_CONNECTOR_HDMIB, HDMIB) ,
  (c'DRM_MODE_CONNECTOR_TV, TV) ,
  (c'DRM_MODE_CONNECTOR_eDP, EDP) ]

type Property = ()
