{-# LANGUAGE Arrows #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad hiding (unless)
import Control.Monad.Random
import Control.Wire hiding ((.), merge)
import Control.Wire.Unsafe.Event hiding (merge)

import Data.List (groupBy, transpose)
import Data.Tuple (swap)
import Data.Char (isAsciiLower)
import Data.Array

import FRP.Netwire.Input

import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L
import Linear hiding (trace, transpose)

import Grid
--------------------------------------------------------------------------------

screenSizeX :: Int
screenSizeX = 800

screenSizeY :: Int
screenSizeY = 600

gameDimX :: Int
gameDimX = 40

gameDimY :: Int
gameDimY = 30

letterSzX :: Int
letterSzX
  | screenSizeX `mod` gameDimX == 0 = screenSizeX `div` gameDimX
  | otherwise = error "Screen size is not multiple of game dimension along x axis"

letterSzY :: Int
letterSzY
  | screenSizeY `mod` gameDimY == 0 = screenSizeY `div` gameDimY
  | otherwise = error "Screen size is not multiple of game dimension along y axis"

numberString :: [Char]
numberString = concat . repeat $ "9876543210"

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x : merge ys xs

mkGameRow :: (Functor m, MonadRandom m) => m String
mkGameRow = filter isAsciiLower <$> getRandoms

mkGameRowWithString :: (Functor m, MonadRandom m) => String -> Int -> m String
mkGameRowWithString str pos = do
  (first, rest) <- splitAt pos <$> mkGameRow
  return $ first ++ merge str rest

startGameString :: (Functor m, MonadRandom m) => m String
startGameString = do
  rc <- head <$> mkGameRow
  mkGameRowWithString (concat ["start", [rc], "game"]) 10

quitGameString :: (Functor m, MonadRandom m) => m String
quitGameString = mkGameRowWithString "quit" 10

gameOverString :: (Functor m, MonadRandom m) => m String
gameOverString = mkGameRowWithString "over" 22

gameBoardString :: (Functor m, MonadRandom m) => m (Grid2D Char)
gameBoardString = do
  firstRows <- replicateM ((gameDimY `div` 2) - 1) mkGameRow
  startGame <- startGameString
  gameOver <- gameOverString
  quit <- quitGameString
  lastRows <- replicateM ((gameDimY `div` 2) - 4) mkGameRow

  let rows = concat [[numberString],
                     firstRows,
                     [startGame, gameOver, quit],
                     lastRows,
                     [numberString]]
  return $ fromLists $ transpose $ take gameDimY $ map (take gameDimX) rows

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

drawChar :: L.Font -> ((Int, Int), Char) -> L.GameMonad ()
drawChar fnt ((x, y), c) =
  L.renderUIString fnt [c] $ fromIntegral <$> (letterPos ^+^ (V2 4 2))
    where
      letterPos = V2 (x * letterSzX) (screenSizeY - (y + 1) * letterSzY)

data LetterInput
  = ChangeImmediately (V3 Float)
  | ChangeGradually Float (V3 Float)
    deriving (Ord, Eq, Show)

data LetterOutput = LetterOutput {
  outputChar :: Char,
  outputColor :: V3 Float,
  outputClick :: Bool
} deriving (Ord, Eq, Show)

type LetterWire = L.GameWire (Event LetterInput) LetterOutput

mkWiresFromChars :: Grid2D Char -> Grid2D LetterWire
mkWiresFromChars chars = generateGrid gameDimX gameDimY $ \x y -> letterWire (V3 1 1 1) ((x, y), (x + 1, y + 1) `get2D` chars)

letterWire :: V3 Float -> ((Int, Int), Char) -> LetterWire
letterWire initialColor (pos, c) = proc x -> do
  color <- handleColor initialColor -< x
  isClicked <- ((mousePressed GLFW.MouseButton'1 >>> mouseCursor >>> inLetterSpace pos) <|> (pure False)) -< ()
  returnA -< (LetterOutput c color isClicked)
  where
    inLetterSpace :: (Int, Int) -> L.GameWire (Float, Float) Bool
    inLetterSpace (x, y) = mkSF_ $ \(mx, my) ->
      let (V2 minx maxx) = fromIntegral . (*letterSzX) <$> (V2 x (x + 1))
          (V2 miny maxy) = fromIntegral . (*letterSzY) <$> (V2 y (y + 1))
          sx = (mx * 0.5 + 0.5) * (fromIntegral screenSizeX)
          sy = (my * 0.5 + 0.5) * (fromIntegral screenSizeY)
      in
       if (sx >= minx && sx <= maxx && sy >= miny && sy <= maxy)
       then True
       else False

    colorFeedback :: V3 Float -> L.GameWire (Event LetterInput, V3 Float) (V3 Float, V3 Float)
    colorFeedback c =
      let lerpWire :: Float -> V3 Float -> V3 Float -> L.GameWire a (V3 Float)
          lerpWire duration start end = timeF >>>
                                        (arr (/ duration)) >>>
                                        (unless (> 1.0)) >>>
                                        (arr $ \t -> lerp t end start)

          modeSelect :: LetterInput -> L.GameWire (V3 Float) (V3 Float)
          modeSelect (ChangeImmediately newColor) = pure newColor
          modeSelect (ChangeGradually t newColor) =
            mkSFN $ \oldColor -> (oldColor, lerpWire t oldColor newColor --> pure newColor)

      in (arr swap) >>> (modes (ChangeImmediately c) modeSelect) >>> (mkId &&& mkId)

    handleColor :: V3 Float -> L.GameWire (Event LetterInput) (V3 Float)
    handleColor c = loop $ second (delay c) >>> colorFeedback c

type StageInput = Grid2D LetterOutput
type StageOutput = Grid2D (Maybe LetterInput)

type StageWire = L.GameWire StageInput StageOutput

handleLetters :: L.TimeStep -> StageOutput -> Grid2D LetterWire ->
                 L.GameMonad (StageInput, Grid2D LetterWire)
handleLetters ts letterIpts letterWires = do
  unzipGrid <$> (mapGridM runWire $ zipGrid letterIpts letterWires)
  where
    runWire :: (Maybe LetterInput, LetterWire) -> L.GameMonad (LetterOutput, LetterWire)
    runWire (Nothing, wire) = do
      (Right output, newWire) <- stepWire wire ts (Right NoEvent)
      return (output, newWire)
    runWire (Just ipt, wire) = do
      (Right output, newWire) <- stepWire wire ts (Right $ Event ipt)
      return (output, newWire)

renderLetters :: L.Font -> Grid2D LetterOutput -> L.GameMonad ()
renderLetters font grid = imapGridM_ (\ix (LetterOutput c color _) ->
                                       let newFont = L.setFontColor color font
                                       in drawChar newFont (ix, c)) grid
--  let charGrps = groupBy (\(LetterOutput _ x _) (LetterOutput _ y _) -> x == y) $ toLists grid
--  in mapM_ (\(chars@((LetterOutput _ color _) : _)) ->
--              let newFont = L.setFontColor color font
--              in mapM_ (\(ix, LetterOutput c _ _) -> drawChar newFont (ix, c)) chars) charGrps

mkGame :: StageWire -> IO (L.GameWire () ())
mkGame w = do
  board <- gameBoardString
  let initialLetters = mkWiresFromChars board
      dummyStageOutput = (\c -> LetterOutput c (V3 1 1 1) False) <$> board
  font <- L.loadTTFont 18 (V3 1 1 1) "kenpixel.ttf"
  return $ runStage font initialLetters dummyStageOutput w
    where
      runStage :: L.Font -> Grid2D LetterWire -> StageInput -> StageWire -> L.GameWire () ()
      runStage font letters ipt w = mkGen $ \s _ -> do
        (result, nextStage) <- stepWire w s (Right ipt)
        case result of
          Left x -> return (Left x, mkEmpty)
          Right letterInput -> do
            (nextIpt, nextLetters) <- handleLetters s letterInput letters
            renderLetters font nextIpt
            return $ (Right (), runStage font nextLetters nextIpt nextStage)

clickYellow :: StageWire
clickYellow = mkSF_ (changeClicked <$>)
  where
    changeClicked :: LetterOutput -> Maybe LetterInput
    changeClicked (LetterOutput _ _ False) = Nothing
--    changeClicked (LetterOutput _ _ True) = Just $ ChangeGradually 0.3 (V3 0 1 1)
    changeClicked (LetterOutput _ _ True) = Just $ ChangeImmediately (V3 0 1 1)    

initGame :: IO (L.Game ())
initGame = do
  g <- mkGame clickYellow
  return $ L.Game {
    L.staticLights = [],
    L.staticGeometry = [],
    L.mainCamera = camera,
    L.dynamicLights = [],
    L.gameLogic = g >>> (L.quitWire GLFW.Key'Q) }

main :: IO ()
main = do
  m <- L.makeWindow screenSizeX screenSizeY "Literate"
  g <- initGame
  case m of
    (Just win) -> L.run win () g
    Nothing -> return ()
  L.destroyWindow m
