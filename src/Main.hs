-- --cam 0 --width 640 --height 360
-- --cam 1 --width 640 --height 480
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad (unless, void, forM, forM_, when)
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
import Foreign.Ptr (castPtr, plusPtr)
import Data.IORef
import Data.Monoid ((<>))
import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToPrim)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Storable (peek, poke)
import qualified OpenCV.Internal.Mutable as CV
import Data.Foldable (toList)
import Foreign.C (CFloat)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Class (lift)

data Options = Options
  { optsCamId :: Int32
  , optsWidth :: Int
  , optsHeight :: Int
  , optsFps :: Int
  } deriving (Eq, Show)

type Filter = CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8) -> CV.CvExcept (CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8))

run :: [(String, Filter)] -> IO ()
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
            cap <- CV.newVideoCapture
            -- Open the first available video capture device. Usually the
            -- webcam if run on a laptop.
            CV.exceptErrorIO (CV.videoCaptureOpen cap (CV.VideoDeviceSource optsCamId))
            isOpened <- CV.videoCaptureIsOpened cap
            unless isOpened (fail "Couldn't open video capture device")
            writeIORef mbCapRef (Just cap)
            return (cap, True)
          Just cap -> return (cap, False)
        -- Reading image
        _ok <- CV.videoCaptureGrab cap
        mbImg <- CV.videoCaptureRetrieve cap
        case mbImg of
          Nothing -> putStrLn "Did not get an image, quitting"
          Just img0 -> do
            when firstFrame $ do
              putStrLn ("Original image dimensions: " ++ show (CV.miShape (CV.matInfo img0)))
            -- Assert that the retrieved frame is 2-dimensional 3-chan
            let img1 :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img1 = CV.exceptError (CV.coerceMat img0)
            -- Apply filters
            let applyFilters :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8) -> [FLTK.Ref FLTK.Choice] -> CV.CvExceptT IO (CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8))
                applyFilters !img = \case
                  [] -> return img
                  choice : choices_ -> do
                    txt <- lift (FLTK.getText choice)
                    img' <- if null txt
                      then return img
                      else case lookup txt filters of
                        Nothing -> lift (fail ("Could not find filter " ++ show txt))
                        Just f -> CV.pureExcept (f img)
                    applyFilters img' choices_
            img2 <- do
              mbImg <- runExceptT (applyFilters img1 choices)
              case mbImg of
                Left err -> do
                  putStrLn ("Some filter failed, skipping: " ++ show err)
                  return img1
                Right img -> return img
            -- Convert to RGB
            let img3 :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img3 = CV.exceptError (CV.cvtColor CV.bgr CV.rgb img2)
            -- Resize the image to 100x100
            let img4 :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
                img4 = CV.exceptError (CV.coerceMat =<< CV.resize (CV.ResizeAbs (CV.toSize (L.V2 (fromIntegral optsWidth) (fromIntegral optsHeight)))) CV.InterArea img3)
            CV.withMatData img4 $ \_sizes ptr -> do
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

blur :: Filter
blur = CV.gaussianBlur (0 :: L.V2 Int32) 5 0

circles :: Filter
circles img = do
  gray <- CV.cvtColor CV.rgb CV.gray img
  let cs = CV.houghCircles 1 10 Nothing Nothing Nothing Nothing gray
  CV.createMat $ do
    imgM <- CV.thaw img
    forM_ cs $ \c -> do
      let color :: L.V4 Double = L.V4 0 0 255 255
      CV.circle imgM (round <$> CV.circleCenter c :: L.V2 Int32) (round (CV.circleRadius c)) color 1 CV.LineType_AA 0
    return imgM

grayscale :: Filter
grayscale img =
  CV.cvtColor CV.gray CV.rgb =<< CV.cvtColor CV.rgb CV.gray img

greenScreenMask :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8) -> CV.CvExcept (CV.Mat ('S ['D, 'D]) ('S 1) ('S Word8))
greenScreenMask img = do
  let s :: L.V4 Double = L.V4 0 255 0 255
  CV.inRange img s s

manga :: CV.CascadeClassifier -> MaskedImage -> MaskedImage -> Filter
manga cc leftMangaEye rightMangaEye img = do
  imgGray <- CV.cvtColor CV.rgb CV.gray img
  let eyes = CV.cascadeClassifierDetectMultiScale cc Nothing Nothing (Nothing :: Maybe (L.V2 Int32)) (Nothing :: Maybe (L.V2 Int32)) imgGray
  case map CV.fromRect (toList eyes) of
    eye1 : eye2 : _rest -> do
      let rectX r = let L.V2 x _ = CV.hRectTopLeft r in x
      let rectCenter :: CV.HRect Int32 -> L.V2 Double
          rectCenter (CV.HRect (L.V2 x y) (L.V2 w h)) = L.V2 (fromIntegral x + fromIntegral w / 2) (fromIntegral y + fromIntegral h / 2)
      let (leftEye, rightEye) = if rectX eye1 < rectX eye2 then (eye1, eye2) else (eye2, eye1)
      let leftEyeCenter = rectCenter leftEye
      let rightEyeCenter = rectCenter rightEye
      let leftToRight@(L.V2 ltrX ltrY) = rightEyeCenter - leftEyeCenter
      let dist = L.norm leftToRight
      let scale = dist / mangaEyesDistance
      let rot = atan2 ltrY ltrX
      CV.createMat $ do
        imgM <- CV.thaw img
        --let color :: L.V4 Double = L.V4 0 0 255 255
        --CV.line imgM (round <$> rectCenter leftEye) (round <$> rectCenter rightEye) color 5 CV.LineType_AA 0
        scaledLeftEye <- CV.pureExcept $ scaleMaskedImage scale =<< rotateMaskedImage rot leftMangaEye
        copyMaskedImageTo imgM (fmap round (leftEyeCenter - matCenter (miImage scaledLeftEye))) scaledLeftEye
        scaledRightEye <- CV.pureExcept $ scaleMaskedImage scale =<< rotateMaskedImage rot rightMangaEye
        copyMaskedImageTo imgM (fmap round (rightEyeCenter - matCenter (miImage scaledRightEye))) scaledRightEye
        return imgM
    _ -> return img

mangaEyesDistance :: Double
mangaEyesDistance = 371

data MaskedImage = MaskedImage
  { miImage :: CV.Mat ('S ['D, 'D]) ('S 3) ('S Word8)
  , miMask :: CV.Mat ('S ['D, 'D]) ('S 1) ('S Word8)
  }

readMaskedImage :: FilePath -> CV.CvExceptT IO MaskedImage
readMaskedImage fp = do
  me1 :: CV.Mat ('S ['D, 'D]) 'D 'D <- CV.imdecode CV.ImreadUnchanged <$> lift (BS.readFile fp)
  me2 :: CV.Mat ('S ['D, 'D]) ('S 4) ('S Word8) <- CV.pureExcept (CV.coerceMat me1)
  me3 <- CV.pureExcept (CV.cvtColor CV.bgra CV.rgb me2)
  mask :: CV.Mat ('S ['D, 'D]) ('S 1) ('S Word8) <- CV.pureExcept (CV.bitwiseNot =<< greenScreenMask me3)
  return (MaskedImage me3 mask)

scaleMaskedImage :: Double -> MaskedImage -> CV.CvExcept MaskedImage
scaleMaskedImage s MaskedImage{..} = MaskedImage
  <$> CV.resize (CV.ResizeRel (L.V2 s s)) CV.InterArea miImage
  <*> CV.resize (CV.ResizeRel (L.V2 s s)) CV.InterArea miMask

rotateMaskedImage :: Double -> MaskedImage -> CV.CvExcept MaskedImage
rotateMaskedImage ang0 MaskedImage{..} = MaskedImage
  <$> CV.warpAffine miImage (CV.getRotationMatrix2D center ang 1) CV.InterArea False False (CV.BorderConstant (CV.toScalar (L.V4 0 0 0 0 :: L.V4 Double)))
  <*> CV.warpAffine miMask (CV.getRotationMatrix2D center ang 1) CV.InterArea False False (CV.BorderConstant (CV.toScalar (L.V4 0 0 0 0 :: L.V4 Double)))
  where
    ang = -(ang0 * 180 / pi)

    center :: L.V2 CFloat
    center = matCenter miImage

matCenter :: (Fractional a) => CV.Mat ('CV.S [h, w]) chans depth -> L.V2 a
matCenter mat = let
  [rows, cols] = CV.miShape (CV.matInfo mat)
  in L.V2 (fromIntegral cols / 2) (fromIntegral rows / 2)

copyMaskedImageTo ::
     (PrimMonad m)
  => CV.Mut (CV.Mat ('CV.S '[dstHeight, dstWidth]) ('CV.S 3) ('CV.S Word8)) (PrimState m)
  -> L.V2 Int32
  -> MaskedImage
  -> CV.CvExceptT m ()
copyMaskedImageTo dst from@(L.V2 fromX fromY) mi = do
  let [dstRows, dstCols] = CV.miShape (CV.matInfo (CV.unMut dst))
  let [srcRows, srcCols] = CV.miShape (CV.matInfo (miImage mi))
  when (fromX >= 0 && fromX + srcCols <= dstCols && fromY >= 0 && fromY + srcRows <= dstRows) $
    CV.matCopyToM dst from (miImage mi) (Just (miMask mi))

main :: IO ()
main = do
  Just ccEyes <- CV.newCascadeClassifier "haarcascade_eye.xml"
  leftMangaEye <- CV.exceptErrorIO (readMaskedImage "manga-eyes-left.png")
  rightMangaEye <- CV.exceptErrorIO (readMaskedImage "manga-eyes-right.png")
  run
    [ ("none", return)
    , ("blur", blur)
    , ("circles", circles)
    , ("lines", return)
    , ("edges", return)
    , ("grayscale", grayscale)
    , ("floodfill", return)
    , ("manga", manga ccEyes leftMangaEye rightMangaEye)
    ]

