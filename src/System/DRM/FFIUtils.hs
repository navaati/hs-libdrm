module System.DRM.FFIUtils where

import Prelude.Unicode
import Data.Bits
import Data.Maybe(fromJust)



cToFlags ∷ (Bits f, Eq e) ⇒ [(e,f)] → f → [e]
cToFlags table cword = filter (isFlagSet ∘ eToF) $ map fst table
  where eToF = fromJust ∘ flip lookup table
        isFlagSet f = (f .&. cword) ≡ f
