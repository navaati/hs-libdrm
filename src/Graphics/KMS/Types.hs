{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.KMS.Types where

import Foreign
import System.Posix

newtype Drm = Drm Fd
newtype FbId = FbId Word32 deriving (Show, Storable, Eq)
nullFbId :: FbId -> Bool
nullFbId = (==FbId 0)
newtype CrtcId = CrtcId Word32 deriving (Show, Storable, Eq)
newtype ConnectorId = ConnectorId Word32 deriving (Show, Storable, Eq)
newtype EncoderId = EncoderId Word32 deriving (Show, Storable, Eq)
