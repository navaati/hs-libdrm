{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module System.DRM.BufferObject where

import Data.Word

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


type Pitch = Word32
type BPP = Word8
type Depth = Word8
type Size = Word64
