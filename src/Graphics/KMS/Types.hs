{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.KMS.Types where

import Foreign
import System.Posix

newtype Drm = Drm Fd
newtype FbId = FbId Word32 deriving (Show, Storable, Eq)
newtype CrtcId = CrtcId Word32 deriving (Show, Storable, Eq)
newtype ConnectorId = ConnectorId Word32 deriving (Show, Storable, Eq)
newtype EncoderId = EncoderId Word32 deriving (Show, Storable, Eq)
