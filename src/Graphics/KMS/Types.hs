{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.KMS.Types where

import Foreign
import System.Posix

newtype Drm = Drm Fd
newtype FbId drm = FbId Word32 deriving (Show, Storable, Eq)
newtype CrtcId drm = CrtcId Word32 deriving (Show, Storable, Eq)
newtype ConnectorId drm = ConnectorId Word32 deriving (Show, Storable, Eq)
newtype EncoderId drm = EncoderId Word32 deriving (Show, Storable, Eq)
