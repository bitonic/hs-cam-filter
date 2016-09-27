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

import Control.Monad (forM_, (>=>))
import Linear
import Data.Foldable (toList)
import Control.Lens

import Utils

blur :: Filter
blur = gaussianBlur 5

circles :: Filter
circles img = do
  grayImg <- cvtColor rgb gray img
  let cs = houghCircles grayImg
  createMat $ do
    imgM <- thaw img
    forM_ cs $ \c -> do
      circle imgM (round <$> circleCenter c) (round (circleRadius c)) red 3
    return imgM

grayscale :: Filter
grayscale = cvtColor rgb gray >=> cvtColor gray rgb

-- TODO add mask erosion
edges :: Filter
edges img = do
  mask <- canny img
  let imgInfo = matInfo img
  imgRed <- coerceMat =<< mkMat (miShape imgInfo) (miChannels imgInfo) (miDepth imgInfo) red
  createMat $ do
    imgM <- thaw img
    safeMatCopyTo imgM 0 imgRed mask
    return imgM

manga :: CascadeClassifier -> MaskedImage -> MaskedImage -> Filter
manga cc leftMangaEye rightMangaEye img = do
  imgGray <- cvtColor rgb gray img
  let eyes = cascadeClassifierDetectMultiScale cc imgGray
  case map fromRect (toList eyes) of
    eye1 : eye2 : _rest -> do
      let (leftEye, rightEye) = if hRectTopLeft eye1 ^. _x < hRectTopLeft eye2 ^. _x
            then (eye1, eye2)
            else (eye2, eye1)
      let leftEyeCenter = rectCenter leftEye
      let rightEyeCenter = rectCenter rightEye
      let leftToRight = rightEyeCenter - leftEyeCenter
      let dist = norm leftToRight
      let scale = dist / mangaEyesDistance
      let rot = atan2 (leftToRight ^. _y) (leftToRight ^. _x)
      scaledLeftEye <- scaleMaskedImage scale =<< rotateMaskedImage rot leftMangaEye
      scaledRightEye <- scaleMaskedImage scale =<< rotateMaskedImage rot rightMangaEye
      createMat $ do
        imgM <- thaw img
        line imgM (round <$> rectCenter leftEye) (round <$> rectCenter rightEye) blue 5
        copyMaskedImageTo imgM (round <$> (leftEyeCenter - maskedImageCenter scaledLeftEye)) scaledLeftEye
        copyMaskedImageTo imgM (round <$> (rightEyeCenter - maskedImageCenter scaledRightEye)) scaledRightEye
        return imgM
    _ -> return img

mangaEyesDistance :: Double
mangaEyesDistance = 371

main :: IO ()
main = do
  Just ccEyes <- newCascadeClassifier "haarcascade_eye.xml"
  leftMangaEye <- exceptErrorIO (readMaskedImage "manga-eyes-left.png")
  rightMangaEye <- exceptErrorIO (readMaskedImage "manga-eyes-right.png")
  run
    [ ("none", return)
    , ("grayscale", grayscale)
    , ("blur", blur)
    , ("circles", circles)
    , ("edges", edges)
    , ("manga", manga ccEyes leftMangaEye rightMangaEye)
    ]

