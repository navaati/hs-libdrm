{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module System.DRM.BufferObject where

import Foreign(Ptr)

import System.DRM.Types

class BufferObject bo drm | bo → drm where
  boHandle ∷ bo → BOHandle drm
  boSize ∷ bo → Size
  boDestroy ∷ bo → IO ()

class (BufferObject bo drm) ⇒ ImageBO bo drm | bo → drm where
  boRes ∷ bo → (Width, Height)
  boPitch ∷ bo → Pitch
  boBPP ∷ bo → BPP
  boDepth ∷ bo → Depth

class (BufferObject bo drm) ⇒ MappableBO bo drm | bo → drm where
  boMap ∷ bo → IO (Ptr a)
