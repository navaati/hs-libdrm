{-# LANGUAGE RecordWildCards #-}

module System.DRM.KMS.Connector (Connector(..),getConnector,Connection(..),isConnected,SubPixel(..),ConnectorType(..),Property(..)) where

import Prelude.Unicode
import Control.Monad.Unicode
import Foreign
import Foreign.C
import Data.Maybe(fromJust)

import System.DRM.C.KMS.Connector
import System.DRM.Types
import System.DRM.KMS.ModeInfo
import System.DRM.C.KMS.ModeInfo

data Connector drm = Connector
                     { connectorId ∷ ConnectorId drm
                     , connectorCurrentEncoder ∷ EncoderId drm
                     , connectorType ∷ ConnectorType
                     , connectorTypeId ∷ Word32
                     , connectorConnection ∷ Connection
                     , mmSize ∷ (Word32,Word32)
                     , connectorSubpixel ∷ SubPixel
                     , connectorModeInfo ∷ [ModeInfo]
                     , connectorProperties ∷ [Property]
                     , connectorEncoders ∷ [EncoderId drm]
                     } deriving (Show)

cToConnector ∷ C'drmModeConnector → IO (Connector drm)
cToConnector (C'drmModeConnector{..}) = do
  let fI = fromIntegral
  cModes ← peekArray
    (fI c'drmModeConnector'count_modes) c'drmModeConnector'modes
  props ← peekArray
    (fI c'drmModeConnector'count_props) c'drmModeConnector'props
  prop_values ← peekArray
    (fI c'drmModeConnector'count_props) c'drmModeConnector'prop_values
  encoders ← peekArray
    (fI c'drmModeConnector'count_encoders) c'drmModeConnector'encoders
  return $ Connector
    (ConnectorId c'drmModeConnector'connector_id)
    (EncoderId c'drmModeConnector'encoder_id)
    (fromJust $ lookup c'drmModeConnector'connector_type
     connectorTypeEnum)
    c'drmModeConnector'connector_type_id
    (fromJust $ lookup c'drmModeConnector'connection connectionEnum)
    (c'drmModeConnector'mmWidth,c'drmModeConnector'mmHeight)
    (fromJust $ lookup c'drmModeConnector'subpixel subpixelEnum)
    (fmap cToModeInfo cModes)
    (zipWith (const ∘ const ()) props prop_values)
    (fmap EncoderId encoders)

getConnector ∷ (RDrm drm) ⇒
                ConnectorId drm → IO (Connector drm)
getConnector cId = do
  ptr ← throwErrnoIfNull "drmModeGetConnector" $
         applyDrm c'drmModeGetConnector cId
  connector ← cToConnector =≪ peek ptr
  c'drmModeFreeConnector ptr
  return connector

data Connection = Connected | Disconnected | UnknownConnection deriving (Show, Eq)

connectionEnum ∷ [(C'drmModeConnection,Connection)]
connectionEnum = [
  (c'DRM_MODE_CONNECTED, Connected) ,
  (c'DRM_MODE_DISCONNECTED, Disconnected) ,
  (c'DRM_MODE_UNKNOWNCONNECTION, UnknownConnection) ]

isConnected ∷ Connector drm → Bool
isConnected = (≡ Connected) ∘ connectorConnection

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
