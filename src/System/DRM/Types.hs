{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.DRM.Types where

import Foreign
import System.Posix
import Data.Reflection

newtype Drm = Drm Fd
type RDrm drm = (Reifies drm Drm)
newtype Fb drm = FbId Word32 deriving (Show, Storable, Eq)
newtype Crtc drm = CrtcId Word32 deriving (Show, Storable, Eq)
newtype Connector drm = ConnectorId Word32 deriving (Show, Storable, Eq)
newtype Encoder drm = EncoderId Word32 deriving (Show, Storable, Eq)
newtype BOHandle drm = BOHandle Word32 deriving (Show, Storable, Eq)

type Width = Word32
type Height = Word32
type Pitch = Word32
type BPP = Word8
type Depth = Word8
type Size = Word64

withSameTagAs ∷ a t → b t → a t
withSameTagAs = const

applyDrm ∷ RDrm drm ⇒
  (Drm → i drm → r) → i drm → r
applyDrm f i = f (reflect i) i
