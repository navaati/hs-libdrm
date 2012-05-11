{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module System.DRM.BufferObject where

import Data.Word

import System.DRM.Types

class BufferObject bo where
  type BOHandle bo
  boHandle ∷ bo → BOHandle bo
  boSize ∷ bo → Size
  boDestroy ∷ bo → IO ()

class (BufferObject bo) ⇒ ImageBO bo where
  boRes ∷ bo → (Width, Height)
  boPitch ∷ bo → Pitch
  boBPP ∷ bo → BPP
  boDepth ∷ bo → Depth


type Pitch = Word32
type BPP = Word8
type Depth = Word8
type Size = Word64
