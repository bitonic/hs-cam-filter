{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (unless, void)
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as CV
import OpenCV.TypeLevel
import qualified Options.Applicative as Opts
import Data.Int (Int32)
import qualified Graphics.UI.FLTK.LowLevel.FL as FLTK hiding (redraw)
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK
import Data.Word (Word8)
import qualified Linear as L
import qualified Data.ByteString as BS
import Foreign.Ptr (castPtr)
import Data.IORef
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))

data Options = Options
  { optsCamId :: Int32
  , optsWidth :: Int
  , optsHeight :: Int
  , optsFps :: Int
  } deriving (Eq, Show)

main :: IO ()
main = do
  -- Parse cmd line options
  let parser = do
        optsCamId <- Opts.option Opts.auto (Opts.long "cam" <> Opts.short 'c' <> Opts.value 0)
        optsWidth <- Opts.option Opts.auto (Opts.long "width" <> Opts.short 'w' <> Opts.value 100)
        optsHeight <- Opts.option Opts.auto (Opts.long "height" <> Opts.short 'h' <> Opts.value 100)
        optsFps <- Opts.option Opts.auto (Opts.long "fps" <> Opts.value 30)
        pure Options{..}
  Options{..} <- Opts.execParser (Opts.info (Opts.helper <*> parser) Opts.fullDesc)
  let frameDuration = 1 / fromIntegral optsFps

  mbCapRef <- newIORef Nothing

  -- FLTK
  void (FLTK.visual FLTK.ModeRGB)
  w <- FLTK.windowNew (FLTK.toSize (optsWidth, optsHeight)) Nothing Nothing
  -- Add box where the image will be shown
  imgRef :: IORef BS.ByteString <- newIORef (BS.pack (concat (replicate (optsWidth * optsHeight) [0, 255, 0])))
  let drawImage _ = do
        img <- readIORef imgRef
        FLTK.flcDrawImageBuf img (FLTK.toRectangle (0, 0, optsWidth, optsHeight)) (Just 3) (Just 0)
  box <- FLTK.boxCustom
    (FLTK.toRectangle (0, 0, optsWidth, optsHeight))
    Nothing
    (Just drawImage)
    (Just FLTK.defaultCustomWidgetFuncs)
  let callback = do
        mbCap <- readIORef mbCapRef
        cap <- case mbCap of
          Nothing -> do
            putStrLn "Intializing OpenCV"
            -- Initialize OpenCV
            cap <- CV.newVideoCapture
            -- Open the first available video capture device. Usually the
            -- webcam if run on a laptop.
            CV.exceptErrorIO (CV.videoCaptureOpen cap (CV.VideoDeviceSource optsCamId))
            isOpened <- CV.videoCaptureIsOpened cap
            unless isOpened (fail "Couldn't open video capture device")
            writeIORef mbCapRef (Just cap)
            return cap
          Just cap -> return cap
        -- Reading image
        _ok <- CV.videoCaptureGrab cap
        mbImg <- CV.videoCaptureRetrieve cap
        case mbImg of
          Nothing -> putStrLn "Did not get an image, quitting"
          Just img0 -> do
            -- Assert that the retrieved frame is 2-dimensional 3-chan
            let img1 :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img1 = CV.exceptError (CV.coerceMat img0)
            -- Convert to RGB
            let img2 :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img2 = CV.exceptError (CV.cvtColor (Proxy @'CV.BGR) (Proxy @'CV.RGB) img1)
            -- Resize the image to 100x100
            let img3 :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img3 = CV.exceptError (CV.coerceMat =<< CV.resize (CV.ResizeAbs (CV.toSize (L.V2 (fromIntegral optsWidth) (fromIntegral optsHeight)))) CV.InterArea img2)
            CV.withMatData img3 $ \_sizes ptr -> do
              -- Pack it as a ByteString
              bs <- BS.packCStringLen (castPtr ptr, fromIntegral (optsWidth * optsHeight * 3))
              -- Write it in the ref
              writeIORef imgRef bs
              -- Instruct a redraw
              FLTK.redraw box
        FLTK.repeatTimeout frameDuration callback
  FLTK.addTimeout frameDuration callback
  FLTK.end w
  FLTK.showWidget w
  ret <- FLTK.run
  unless (ret == 0) $
    fail ("FLTK.run exited with a non-zero code: " ++ show ret)
  {-
  -- Go
  cap <- CV.newVideoCapture
  -- Open the first available video capture device. Usually the
  -- webcam if run on a laptop.
  CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource optsCamId
  isOpened <- CV.videoCaptureIsOpened cap
  case isOpened of
    False -> putStrLn "Couldn't open video capture device"
    True -> CV.withWindow "video" $ \window -> do
              loop cap window
  where
    loop cap window = do
      _ok <- CV.videoCaptureGrab cap
      mbImg <- CV.videoCaptureRetrieve cap
      case mbImg of
        Just img -> do
          -- Assert that the retrieved frame is 2-dimensional.
          let img' :: CV.Mat ('S ['D, 'D]) 'D 'D
              img' = CV.exceptError $ CV.coerceMat img
          CV.imshow window img'
          key <- CV.waitKey 20
          -- Loop unless the escape key is pressed.
          unless (key == 27) $ loop cap window
        -- Out of frames, stop looping.
        Nothing -> pure ()
  -}