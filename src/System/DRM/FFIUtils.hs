{-# LANGUAGE UnicodeSyntax #-}

module System.DRM.FFIUtils(cToFlags,flagsToC) where

import FunctionalTools.Unicode
import Data.Bits
import Data.Maybe(fromJust)
import Data.List(foldl')

cToFlags ∷ (Bits f, Eq e) ⇒ [(e,f)] → f → [e]
cToFlags table cword = filter (isFlagSet ∘ eToF table) $ map fst table
  where isFlagSet f = (f .&. cword) ≡ f

flagsToC ∷ (Bits f, Eq e) ⇒ [(e,f)] → [e] → f
flagsToC table = foldl' (.|.) 0 ∘ map (eToF table)

eToF ∷ Eq α ⇒ [(α, γ)] → α → γ
eToF table = fromJust ∘ flip lookup table
