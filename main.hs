import Control.Monad

import Graphics.KMS.Types
import Graphics.KMS.Utils
import Graphics.KMS.Resources
import Graphics.KMS.Connector
import Graphics.KMS.ModeInfo
import Graphics.KMS.Encoder
import Graphics.KMS.Crtc

main :: IO ()
main = withDrm $ do
  (conn,enc,crtc,mode) <- currentRes
  
  print $ conn { connectorModeInfo = []}
  lf >> print enc
  lf >> print crtc
  lf >> print mode
  
  _ <- getLine
  return ()

currentRes :: (?drm :: Drm) ⇒ IO (Connector,Encoder,Crtc,ModeInfo)
currentRes = do
  conn ← (fmap (head . filter isConnected) . mapM getConnector . resConnectors)
    =<< getResources
  enc <- getEncoder $ connectorCurrentEncoder conn
  crtc <- getCrtc $ encoderCrtcId enc
  return (conn,enc,crtc,crtcMode crtc)

{-printMode :: ModeInfo -> IO ()
printMode mode = do
  lf
  print mode
  putStr "Flags :\t"
  print . showBits $ modeFlags mode
  putStr "Type :\t"
  print . showBits $ modeType mode-}

lf :: IO ()
lf = putStrLn ""
