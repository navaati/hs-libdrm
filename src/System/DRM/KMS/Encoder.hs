{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module System.DRM.KMS.Encoder
       ( encoderType
       , encoderCrtc
       , encoderPossibleCrtcs
       , encoderPossibleClones
       , EncoderType(..)
       ) where

import FunctionalTools.Unicode
import Foreign
import Foreign.C.Error
import Data.Maybe(fromJust)

import System.DRM.C.KMS.Encoder
import System.DRM.Types

encoderType ∷ (RDrm drm) ⇒ Encoder drm → IO EncoderType
encoderType =
  fmap (fromJust ∘ flip lookup encoderTypeEnum ∘ c'drmModeEncoder'encoder_type)
  ∘ getEncoder

encoderCrtc ∷ (RDrm drm) ⇒ Encoder drm → IO (CrtcId drm)
encoderCrtc = fmap (CrtcId ∘ c'drmModeEncoder'crtc_id)
              ∘ getEncoder

encoderPossibleCrtcs ∷ (RDrm drm) ⇒ Encoder drm → IO Word32
encoderPossibleCrtcs = fmap c'drmModeEncoder'possible_crtcs ∘ getEncoder

encoderPossibleClones ∷ (RDrm drm) ⇒ Encoder drm → IO Word32
encoderPossibleClones = fmap c'drmModeEncoder'possible_clones ∘ getEncoder

getEncoder ∷ (RDrm drm) ⇒
  Encoder drm → IO C'drmModeEncoder
getEncoder eId = do
  ptr ← throwErrnoIfNull "drmModeGetEncoder" $
         applyDrm c'drmModeGetEncoder eId
  encoder ← peek ptr
  c'drmModeFreeEncoder ptr
  return encoder

data EncoderType = EncoderNone | DAC | TMDS | EncoderLVDS | TVDAC deriving (Show, Eq)

encoderTypeEnum ∷ [(Word32,EncoderType)]
encoderTypeEnum = [
  (c'DRM_MODE_ENCODER_NONE, EncoderNone),
  (c'DRM_MODE_ENCODER_DAC, DAC),
  (c'DRM_MODE_ENCODER_TMDS, TMDS),
  (c'DRM_MODE_ENCODER_LVDS, EncoderLVDS),
  (c'DRM_MODE_ENCODER_TVDAC, TVDAC)]
