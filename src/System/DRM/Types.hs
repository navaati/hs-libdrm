{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.DRM.Types where

import Foreign
import System.Posix
import Data.Proxy
import Data.Reflection

newtype Drm = Drm Fd
type RDrm drm = (Reifies drm Drm)
newtype FbId drm = FbId Word32 deriving (Show, Storable, Eq)
newtype CrtcId drm = CrtcId Word32 deriving (Show, Storable, Eq)
newtype ConnectorId drm = ConnectorId Word32 deriving (Show, Storable, Eq)
newtype EncoderId drm = EncoderId Word32 deriving (Show, Storable, Eq)

type Width = Word32
type Height = Word32

withSameTagAs ∷ a t -> b t -> a t
withSameTagAs = const

sameTagProxy ∷ a t -> Proxy t
sameTagProxy = withSameTagAs Proxy

applyDrm ∷ RDrm drm ⇒
  (Drm → i drm → r) → i drm → r
applyDrm f i = f (reflect $ sameTagProxy i) i
