{-# LANGUAGE ForeignFunctionInterface #-}

module System.DRM.KMS.Connector where

import Prelude.Unicode
import Foreign
import Foreign.C
import System.Posix

#include<stdint.h>
#include<xf86drmMode.h>

import System.DRM.Types
import System.DRM.KMS.ModeInfo
import System.DRM.FFIUtils

data Connector drm = Connector
                     { connectorId ∷ ConnectorId drm
                     , connectorCurrentEncoder ∷ EncoderId drm
                     , connectorType ∷ ConnectorType
                     , connectorTypeId ∷ Word32
                     , connectorConnection ∷ Connection
                     , mmSize ∷ (Width,Height)
                     , connectorSubpixel ∷ SubPixel
                     , connectorModeInfo ∷ [ModeInfo]
                     , connectorProperties ∷ [Property]
                     , connectorEncoders ∷ [EncoderId drm]
                     } deriving (Show)

#define hsc_p(field) hsc_peek(drmModeConnector, field)

peekConnector ∷ ConnectorPtr drm → IO (Connector drm)
peekConnector ptr = do
  cId ← (#p connector_id) ptr
  currEncoder ← (#p encoder_id) ptr
  cType ← (#p connector_type) ptr
  typeId ← (#p connector_type_id) ptr
  connection ← (#p connection) ptr
  width ← (#p mmWidth) ptr
  height ← (#p mmHeight) ptr
  subpixel ← (#p subpixel) ptr
  modes ← lPeekArray ptr (#p count_modes) (#p modes)
  propertiesCount ← (#p count_props) ptr
  let properties = replicate propertiesCount ()
  encoders ← lPeekArray ptr (#p count_encoders) (#p encoders)
  return $ Connector cId currEncoder cType typeId connection
    (width,height) subpixel modes properties encoders

getConnector ∷ (RDrm drm) ⇒
                ConnectorId drm → IO (Connector drm)
getConnector cId = do
  ptr ← throwErrnoIfNull "drmModeGetConnector" $
         applyDrm drmModeGetConnector cId
  connector ← peekConnector ptr
  drmModeFreeConnector ptr
  return connector

data Connection = Connected | Disconnected | UnknownConnection deriving (Show, Eq)

connectionEnum ∷ [(CInt,Connection)]
connectionEnum = [
  ((#const DRM_MODE_CONNECTED), Connected) ,
  ((#const DRM_MODE_DISCONNECTED), Disconnected) ,
  ((#const DRM_MODE_UNKNOWNCONNECTION), UnknownConnection) ]

instance Storable Connection where
  sizeOf _ = sizeOf (undefined ∷ CInt)
  alignment _ = alignment (undefined ∷ CInt)
  peek = peekEnum connectionEnum
  poke = undefined

isConnected ∷ Connector drm → Bool
isConnected = (≡ Connected)∘connectorConnection

data SubPixel = UnknownSubPixel | HorizontalRGB | HorizontalBGR | VerticalRGB | VerticalBGR | None deriving (Show, Eq)

subpixelEnum ∷ [(CInt,SubPixel)]
subpixelEnum = [
  ((#const DRM_MODE_SUBPIXEL_UNKNOWN), UnknownSubPixel) ,
  ((#const DRM_MODE_SUBPIXEL_HORIZONTAL_RGB), HorizontalRGB) ,
  ((#const DRM_MODE_SUBPIXEL_HORIZONTAL_BGR), HorizontalBGR) ,
  ((#const DRM_MODE_SUBPIXEL_VERTICAL_RGB), VerticalRGB) ,
  ((#const DRM_MODE_SUBPIXEL_VERTICAL_BGR), VerticalBGR) ,
  ((#const DRM_MODE_SUBPIXEL_NONE), None) ]

instance Storable SubPixel where
  sizeOf _ = sizeOf (undefined ∷ CInt)
  alignment _ = alignment (undefined ∷ CInt)
  peek = peekEnum subpixelEnum
  poke = undefined

data ConnectorType = UnknownConnectorType | VGA | DVII | DVID | DVIA | Composite | SVIDEO | LVDS | Component | NinePinDIN | DisplayPort | HDMIA | HDMIB | TV | EDP deriving (Show, Eq)

connectorTypeEnum ∷ [(Word32,ConnectorType)]
connectorTypeEnum = [
  ((#const DRM_MODE_CONNECTOR_Unknown), UnknownConnectorType) ,
  ((#const DRM_MODE_CONNECTOR_VGA), VGA) ,
  ((#const DRM_MODE_CONNECTOR_DVII), DVII) ,
  ((#const DRM_MODE_CONNECTOR_DVID), DVID) ,
  ((#const DRM_MODE_CONNECTOR_DVIA), DVIA) ,
  ((#const DRM_MODE_CONNECTOR_Composite), Composite) ,
  ((#const DRM_MODE_CONNECTOR_SVIDEO), SVIDEO) ,
  ((#const DRM_MODE_CONNECTOR_LVDS), LVDS) ,
  ((#const DRM_MODE_CONNECTOR_Component), Component) ,
  ((#const DRM_MODE_CONNECTOR_9PinDIN), NinePinDIN) ,
  ((#const DRM_MODE_CONNECTOR_DisplayPort), DisplayPort) ,
  ((#const DRM_MODE_CONNECTOR_HDMIA), HDMIA) ,
  ((#const DRM_MODE_CONNECTOR_HDMIB), HDMIB) ,
  ((#const DRM_MODE_CONNECTOR_TV), TV) ,
  ((#const DRM_MODE_CONNECTOR_eDP), EDP) ]

instance Storable ConnectorType where
  sizeOf _ = sizeOf (undefined ∷ Word32)
  alignment _ = alignment (undefined ∷ Word32)
  peek = peekEnum connectorTypeEnum
  poke = undefined

type Property = ()

type ConnectorPtr drm = Ptr (Connector drm)

foreign import ccall "drmModeGetConnector"
  drmModeGetConnector ∷ Drm → ConnectorId drm → IO (ConnectorPtr drm)
foreign import ccall "drmModeFreeConnector"
  drmModeFreeConnector ∷ ConnectorPtr drm → IO ()
