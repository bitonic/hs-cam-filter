{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Utils
  ( -- * OpenCV + goodies
    module OpenCV
  , houghCircles
  , rectCenter
  , safeMatCopyTo
  , gaussianBlur
  , cascadeClassifierDetectMultiScale
  , canny
  , circle
  , line
  , matCenter
  , modifyMat
    -- ** Colors
  , blue
  , red

    -- * Filters
  , Filter
    -- * Masked images
  , MaskedImage
  , readMaskedImage
  , rotateMaskedImage
  , scaleMaskedImage
  , maskedImageCenter
  , copyMaskedImageTo

    -- * Main entry point
  , run

    -- * Extras
  , module Control.Monad
  , module Control.Lens
  , module Data.Foldable
  , module Linear
  ) where

import Data.Foldable
import Control.Monad
import Control.Lens hiding (from, ix)
import OpenCV hiding (norm, blur, normalize, gaussianBlur, houghCircles, cascadeClassifierDetectMultiScale, canny, circle, line)
import qualified OpenCV as CV
import OpenCV.Internal.Core.Types.Mat
import qualified Options.Applicative as Opts
import Data.Int (Int32)
import qualified Graphics.UI.FLTK.LowLevel.FL as FLTK hiding (redraw)
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types as FLTK
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FLTK
import Data.Word
import Linear
import qualified Data.ByteString as BS
import Foreign.Ptr (castPtr)
import Data.IORef
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Monad.Primitive (PrimMonad, PrimState)
import OpenCV.Internal.Mutable
import Foreign.C (CFloat)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V

-- * OpenCV + goodies
-----------------------------------------------------------------------

houghCircles :: Mat ('S '[w, h]) ('S 1) ('S Word8) -> V.Vector Circle
houghCircles = CV.houghCircles 1 10 Nothing Nothing Nothing Nothing

rectCenter :: HRect Int32 -> V2 Double
rectCenter (HRect (V2 x y) (V2 w h)) = V2 (fromIntegral x + fromIntegral w / 2) (fromIntegral y + fromIntegral h / 2)

gaussianBlur ::
     (depth `In` '[Word8, Word16, Float, Double])
  => Double
  -> Mat shape ('S channels) ('S depth)
  -> CvExcept (Mat shape ('S channels) ('S depth))
gaussianBlur s = CV.gaussianBlur (0 :: V2 Int32) s 0

cascadeClassifierDetectMultiScale ::
     CascadeClassifier
  -> Mat ('S [w, h]) ('S 1) ('S Word8)
  -> V.Vector (Rect Int32)
cascadeClassifierDetectMultiScale cc =
  CV.cascadeClassifierDetectMultiScale cc Nothing Nothing (Nothing :: Maybe (V2 Int32)) (Nothing :: Maybe (V2 Int32))

canny ::
     Mat ('S [w, h]) channels ('S Word8)
  -> CvExcept (Mat ('S [w, h]) ('S 1) ('S Word8))
canny = CV.canny 30 200 Nothing CannyNormL1

circle ::
     (PrimMonad m, IsPoint2 point2 Int32, ToScalar color)
  => Mut (Mat ('S '[height, width]) channels depth) (PrimState m)
  -> point2 Int32
  -> Int32
  -> color
  -> Int32
  -> m ()
circle img center r color thickness = CV.circle img center r color thickness CV.LineType_AA 0

line
    :: ( PrimMonad m
       , IsPoint2 fromPoint2 Int32
       , IsPoint2 toPoint2   Int32
       , ToScalar color
       )
    => Mut (Mat ('S [height, width]) channels depth) (PrimState m) -- ^ Image.
    -> fromPoint2 Int32 -- ^ First point of the line segment.
    -> toPoint2   Int32 -- ^ Scond point of the line segment.
    -> color -- ^ Line color.
    -> Int32 -- ^ Line thickness.
    -> m ()
line img pt1 pt2 color thickness = CV.line img pt1 pt2 color thickness CV.LineType_AA 0

safeMatCopyTo ::
     (PrimMonad m)
  => Mut (Mat ('S '[dstHeight, dstWidth]) channels ('S Word8)) (PrimState m)
  -> V2 Int32
  -> Mat ('S ['D, 'D]) channels ('S Word8)
  -> Mat ('S ['D, 'D]) ('S 1) ('S Word8)
  -> CvExceptT m ()
safeMatCopyTo dst from@(V2 fromX fromY) src srcMask = do
  let [dstRows, dstCols] = miShape (matInfo (unMut dst))
  let [srcRows, srcCols] = miShape (matInfo src)
  when (fromX >= 0 && fromX + srcCols <= dstCols && fromY >= 0 && fromY + srcRows <= dstRows) $
    matCopyToM dst from src (Just srcMask)

blue :: V4 Double
blue = V4 0 0 255 255

red :: V4 Double
red = V4 255 0 0 255

modifyMat ::
     Mat shape channels depth
  -> (forall m. (PrimMonad m) => Mut (Mat shape channels depth) (PrimState m) -> CvExceptT m ())
  -> CvExcept (Mat shape channels depth)
modifyMat mat cont = do
  createMat $ do
    matM <- thaw mat
    cont matM
    return matM

-- * Filters
-----------------------------------------------------------------------

type Filter = Mat ('S ['D, 'D]) ('S 3) ('S Word8) -> CvExcept (Mat ('S ['D, 'D]) ('S 3) ('S Word8))

-- * Masked image
-----------------------------------------------------------------------

greenScreenMask :: Mat ('S ['D, 'D]) ('S 3) ('S Word8) -> CvExcept (Mat ('S ['D, 'D]) ('S 1) ('S Word8))
greenScreenMask img = do
  let s :: V4 Double = V4 0 255 0 255
  inRange img s s

data MaskedImage = MaskedImage
  { miImage :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
  , miMask :: Mat ('S ['D, 'D]) ('S 1) ('S Word8)
  }

readMaskedImage :: FilePath -> CvExceptT IO MaskedImage
readMaskedImage fp = do
  me1 :: Mat ('S ['D, 'D]) 'D 'D <- imdecode ImreadUnchanged <$> lift (BS.readFile fp)
  me2 :: Mat ('S ['D, 'D]) ('S 4) ('S Word8) <- pureExcept (coerceMat me1)
  me3 <- pureExcept (cvtColor bgra rgb me2)
  mask :: Mat ('S ['D, 'D]) ('S 1) ('S Word8) <- pureExcept (bitwiseNot =<< greenScreenMask me3)
  return (MaskedImage me3 mask)

scaleMaskedImage :: Double -> MaskedImage -> CvExcept MaskedImage
scaleMaskedImage s MaskedImage{..} = MaskedImage
  <$> resize (ResizeRel (V2 s s)) InterArea miImage
  <*> resize (ResizeRel (V2 s s)) InterArea miMask

rotateMaskedImage :: Double -> MaskedImage -> CvExcept MaskedImage
rotateMaskedImage ang0 MaskedImage{..} = MaskedImage
  <$> warpAffine miImage (getRotationMatrix2D center ang 1) InterArea False False (BorderConstant (toScalar (V4 0 0 0 0 :: V4 Double)))
  <*> warpAffine miMask (getRotationMatrix2D center ang 1) InterArea False False (BorderConstant (toScalar (V4 0 0 0 0 :: V4 Double)))
  where
    ang = -(ang0 * 180 / pi)

    center :: V2 CFloat
    center = matCenter miImage

copyMaskedImageTo ::
     (PrimMonad m)
  => Mut (Mat ('S '[dstHeight, dstWidth]) ('S 3) ('S Word8)) (PrimState m)
  -> V2 Int32
  -> MaskedImage
  -> CvExceptT m ()
copyMaskedImageTo dst from mi = safeMatCopyTo dst from (miImage mi) (miMask mi)

maskedImageCenter :: (Fractional a) => MaskedImage -> V2 a
maskedImageCenter = matCenter . miImage

matCenter :: (Fractional a) => Mat ('S [h, w]) chans depth -> V2 a
matCenter mat = let
  [rows, cols] = miShape (matInfo mat)
  in V2 (fromIntegral cols / 2) (fromIntegral rows / 2)

-- * Main entry point
-----------------------------------------------------------------------

data Options = Options
  { optsCamId :: Int32
  , optsWidth :: Int
  , optsHeight :: Int
  , optsFps :: Int
  } deriving (Eq, Show)

run :: [(T.Text, Filter)] -> IO ()
run filters = do
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
  let topMenuHeight = 20
  void (FLTK.visual FLTK.ModeRGB)
  w <- FLTK.windowNew (FLTK.toSize (optsWidth, topMenuHeight + optsHeight)) Nothing (Just "zombo.com")
  -- Add box where the image will be shown
  imgRef :: IORef BS.ByteString <- newIORef (BS.pack (concat (replicate (optsWidth * optsHeight) [0, 0, 0])))
  let drawImage _ = do
        img <- readIORef imgRef
        FLTK.flcDrawImageBuf img (FLTK.toRectangle (0, topMenuHeight, optsWidth, topMenuHeight + optsHeight)) (Just 3) (Just 0)
  box <- FLTK.boxCustom
    (FLTK.toRectangle (0, topMenuHeight, optsWidth, optsHeight + topMenuHeight))
    Nothing
    (Just drawImage)
    (Just FLTK.defaultCustomWidgetFuncs)
  -- Add filter choices
  let filterChoiceWidth = 100
  choices <- forM [0,1..4] $ \ix -> do
    choice <- FLTK.choiceNew (FLTK.toRectangle (ix*filterChoiceWidth, 0, filterChoiceWidth, topMenuHeight)) Nothing
    forM_ (map fst filters) $ \s ->
      void (FLTK.add choice s Nothing (Nothing :: Maybe (FLTK.Ref FLTK.MenuItem -> IO ())) (FLTK.MenuItemFlags []))
    return choice
  let callback = do
        mbCap <- readIORef mbCapRef
        (cap, firstFrame) <- case mbCap of
          Nothing -> do
            putStrLn "Intializing OpenCV"
            -- Initialize OpenCV
            cap <- newVideoCapture
            -- Open the first available video capture device. Usually the
            -- webcam if run on a laptop.
            exceptErrorIO (videoCaptureOpen cap (VideoDeviceSource optsCamId Nothing))
            isOpened <- videoCaptureIsOpened cap
            unless isOpened (fail "Couldn't open video capture device")
            writeIORef mbCapRef (Just cap)
            return (cap, True)
          Just cap -> return (cap, False)
        -- Reading image
        _ok <- videoCaptureGrab cap
        mbImg <- videoCaptureRetrieve cap
        case mbImg of
          Nothing -> putStrLn "Did not get an image, quitting"
          Just img0 -> do
            when firstFrame $ do
              putStrLn ("Original image dimensions: " ++ show (miShape (matInfo img0)))
            -- Assert that the retrieved frame is 2-dimensional 3-chan
            let img1 :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img1 = exceptError (coerceMat img0)
            -- Convert to RGB
            let img2 :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img2 = exceptError (cvtColor bgr rgb img1)
            -- Apply filters
            let applyFilters :: Mat ('S ['D, 'D]) ('S 3) ('S Word8) -> [FLTK.Ref FLTK.Choice] -> CvExceptT IO (Mat ('S ['D, 'D]) ('S 3) ('S Word8))
                applyFilters !img = \case
                  [] -> return img
                  choice : choices_ -> do
                    txt <- lift (FLTK.getText choice)
                    img' <- if T.null txt
                      then return img
                      else case lookup txt filters of
                        Nothing -> lift (fail ("Could not find filter " ++ show txt))
                        Just f -> pureExcept (f img)
                    applyFilters img' choices_
            img3 <- do
              mbImg' <- runExceptT (applyFilters img2 choices)
              case mbImg' of
                Left err -> do
                  putStrLn ("Some filter failed, skipping: " ++ show err)
                  return img2
                Right img -> return img
            -- Resize the image to 100x100
            let img4 :: Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img4 = exceptError (coerceMat =<< resize (ResizeAbs (toSize (V2 (fromIntegral optsWidth) (fromIntegral optsHeight)))) InterArea img3)
            withMatData img4 $ \_sizes ptr -> do
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
