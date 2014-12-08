{-# LANGUAGE Arrows #-}
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Control.Monad hiding (unless)
import Control.Monad.Random
import Control.Wire as W hiding ((.), merge, id)
import Control.Wire.Unsafe.Event hiding (merge)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Function (on)
import Data.List (groupBy, nubBy)
import Data.Tuple (swap)
import Data.Char (isAsciiLower, isSpace)
import Data.Array

import FRP.Netwire.Input

import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L
import Linear hiding (trace)
import Debug.Trace
import System.IO.Unsafe -- !KLUDGE! ugh
import Grid
--------------------------------------------------------------------------------

type Dict = Set String

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
mkGameRow = filter isAsciiLower <$> getRandomRs ('a', 'z')

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

gameBoardString :: (Functor m, MonadRandom m) => m (Array (Int, Int) Char)
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
      idxdList = zip [1..] $ take gameDimY $ map (zip [1..] . take gameDimX) rows
      setRow row (col, x) = ((col, row), x)
      arrList = concat $ (\(row, list) -> setRow row <$> list) <$> idxdList

  return $ array ((1, 1), (gameDimX, gameDimY)) arrList

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

drawChar :: L.Font -> ((Int, Int), Char) -> L.GameMonad ()
drawChar fnt ((x, y), c) =
  L.renderUIString fnt [c] $ fromIntegral <$> (letterPos ^+^ offset)
    where
      letterPos = V2 ((x - 1) * letterSzX) (screenSizeY - y * letterSzY)
      offset = V2 4 2

data LetterState
  = LetterChanging
  | LetterStatic
  deriving (Ord, Eq, Show, Enum, Bounded)
  
data LetterInput
  = ChangeImmediately !(V3 Float)
  | ChangeGradually !Float !(V3 Float)
    deriving (Ord, Eq, Show)

data LetterOutput = LetterOutput {
  outputChar :: !Char,
  outputColor :: !(V3 Float),
  outputClick :: !Bool,
  outputState :: !LetterState
} deriving (Ord, Eq, Show)

type LetterWire = L.GameWire (Event LetterInput) LetterOutput

mkWiresFromChars :: Array (Int, Int) Char -> Array (Int, Int) (LetterOutput, LetterWire)
mkWiresFromChars arr = array (bounds arr) $ (\(ix, c) ->
                                              let output = LetterOutput c (V3 1 1 1) False LetterStatic
                                                  wire = letterWire (V3 1 1 1) (ix, c)
                                              in (ix, (output, wire))) <$> (assocs arr)

letterWire :: V3 Float -> ((Int, Int), Char) -> LetterWire
letterWire initialColor (pos, c) = proc x -> do
  out <- handleColor initialColor -< x
  returnA -< out
  where
    colorFeedback :: V3 Float -> L.GameWire (Event LetterInput, V3 Float) (LetterOutput, V3 Float)
    colorFeedback init =
      let lerpWire :: Float -> V3 Float -> V3 Float -> L.GameWire a LetterOutput
          lerpWire duration start end = proc x -> do
            t <- timeF / pure duration -< x
            color <- (arr $ \t' -> lerp t' end start) -< t
            returnA -< (LetterOutput c color False LetterChanging)

          pureColor color = proc x -> do
            returnA -< (LetterOutput c color False LetterStatic)

          modeSelect :: LetterInput -> L.GameWire (V3 Float) LetterOutput
          modeSelect (ChangeImmediately newColor) = pureColor newColor
          modeSelect (ChangeGradually t newColor) =
            mkSFN $ \oldColor -> (LetterOutput c oldColor False LetterChanging,
                                  (lerpWire t oldColor newColor >>> for t) --> pureColor newColor)

      in (arr swap) >>> (modes (ChangeImmediately init) modeSelect) >>> (mkId &&& (arr outputColor))

    handleColor :: V3 Float -> L.GameWire (Event LetterInput) LetterOutput
    handleColor c = loop $ second (delay c) >>> colorFeedback c

type StageInput = Array (Int, Int) LetterOutput
type StageOutput = Array (Int, Int) (Maybe LetterInput)

type StageWire = L.GameWire StageInput StageOutput

handleLetters :: L.TimeStep -> StageOutput -> Array (Int, Int) (LetterOutput, LetterWire) ->
                 L.GameMonad (StageInput, Array (Int, Int) (LetterOutput, LetterWire))
handleLetters ts letterIpts letterWires = do
  arr <- mapM runWire $ do
    ix <- indices letterIpts
    let li = letterIpts ! ix
        (lst, lw) = letterWires ! ix
    guard (li /= Nothing || outputState lst == LetterChanging)
    return (ix, (li, lw))
  return $ ((fst <$> letterWires) // (map (fst <$>) arr),
            (letterWires // arr))
  where
    runWire :: (i, (Maybe LetterInput, LetterWire)) -> L.GameMonad (i, (LetterOutput, LetterWire))
    runWire (x, (Nothing, wire)) = do
      (Right output, newWire) <- stepWire wire ts (Right NoEvent)
      return (x, (output, newWire))
    runWire (x, (Just ipt, wire)) = do
      (Right output, newWire) <- stepWire wire ts (Right $ Event ipt)
      return (x, (output, newWire))

renderLetters :: L.Font -> Array (Int, Int) LetterOutput -> L.GameMonad ()
renderLetters font arr =
  let charGrps = groupBy (\(_, LetterOutput _ x _ _) (_, LetterOutput _ y _ _) -> x == y) $ assocs arr
  in mapM_ (\(chars@((_, LetterOutput _ color _ _) : _)) ->
              let newFont = L.setFontColor color font
              in mapM_ (\(ix, LetterOutput c _ _ _) -> drawChar newFont (ix, c)) chars) charGrps

checkClicked :: StageInput -> L.GameMonad StageInput
checkClicked ipt = do
  pressed <- mbIsPressed GLFW.MouseButton'1
  case pressed of
    False -> return ipt
    True -> do
      (mx, my) <- cursor
      -- releaseButton GLFW.MouseButton'1
      let sx = (mx * 0.5 + 0.5) * (fromIntegral screenSizeX)
          sy = (my * 0.5 + 0.5) * (fromIntegral screenSizeY)
          x = ceiling $ sx / (fromIntegral letterSzX)
          y = ceiling $ sy / (fromIntegral letterSzY)
          LetterOutput c color _ st = ipt ! (x, y)
      return $ ipt // [((x, y), LetterOutput c color True st)]

mkGame :: StageWire -> IO (L.GameWire () ())
mkGame w = do
  board <- gameBoardString
  let initialLetters = mkWiresFromChars board
      dummyStageOutput = (\c -> LetterOutput c (V3 1 1 1) False LetterStatic) <$> board
  font <- L.loadTTFont 18 (V3 1 1 1) "kenpixel.ttf"
  return $ runStage font initialLetters dummyStageOutput w
    where
      runStage :: L.Font -> Array (Int, Int) (LetterOutput, LetterWire) -> StageInput -> StageWire -> L.GameWire () ()
      runStage font letters ipt w = mkGen $ \ts _ -> do
        (result, nextStage) <- checkClicked ipt >>= (stepWire w ts . Right)
        case result of
          Left x -> return (Left x, mkEmpty)
          Right letterInput -> do
            (nextIpt, nextLetters) <- handleLetters ts letterInput letters
            renderLetters font nextIpt
            return $ (Right (), runStage font nextLetters nextIpt nextStage)

mkStaticStageInput :: (Int -> Int -> a) -> Array (Int, Int) a
mkStaticStageInput f = array ((1, 1), (gameDimX, gameDimY)) [((x, y), f x y) | x <- [1..gameDimX], y <- [1..gameDimY]]

titlePositions :: [(Int, Int)]
titlePositions = (\(x, y) -> (x + 1, y)) <$> [
  {- L -} (4, 4), (4, 5), (4, 6), (4, 7), (4, 8), (5, 8), (6, 8),
  {- I -} (8, 4), (9, 4), (10, 4), (9, 5), (9, 6), (9, 7), (9, 8), (8, 8), (10, 8),
  {- T -} (12, 4), (13, 4), (14, 4), (13, 5), (13, 6), (13, 7), (13, 8),
  {- E -} (16, 4), (17, 4), (18, 4), (16, 5), (16, 6), (17, 6), (16, 7), (16, 8), (17, 8), (18, 8),
  {- R -} (20, 4), (21, 4), (22, 4), (20, 5), (22, 5), (20, 6), (21, 6), (20, 7), (22, 7), (20, 8), (22, 8),
  {- A -} (24, 4), (25, 4), (26, 4), (24, 5), (26, 5), (24, 6), (25, 6), (26, 6), (24, 7), (26, 7), (24, 8), (26, 8),
  {- T -} (28, 4), (29, 4), (30, 4), (29, 5), (29, 6), (29, 7), (29, 8),
  {- E -} (32, 4), (33, 4), (34, 4), (32, 5), (32, 6), (33, 6), (32, 7), (32, 8), (33, 8), (34, 8)
  ]  

startGamePositions :: [(Int, Int)]
startGamePositions = [(11 + x, 16) | x <- [0,2,4,6,8,12,14,16,18]]

gameOverPositions :: [(Int, Int)]
gameOverPositions = [(23 + x, 16) | x <- [0,2,4,6]] ++ [(23 + x, 17) | x <- [0, 2, 4, 6]]

quitGamePositions :: [(Int, Int)]
quitGamePositions = [(11 + x, 18) | x <- [0,2,4,6]]

idleStage :: StageOutput
idleStage = mkStaticStageInput (\x y -> Nothing)

introSequence :: StageWire
introSequence = pure idleStage >>> delay fadeToMenu >>> for timeToFade
  where
    timeToFade = 2.0
    fadeToMenu = mkStaticStageInput introInput
    fadeToWhite = Just $ ChangeGradually timeToFade (V3 1 1 1)
    introInput :: Int -> Int -> Maybe LetterInput
    introInput 1 _ = fadeToWhite
    introInput _ 1 = fadeToWhite
    introInput x y
      | x == gameDimX = fadeToWhite
      | y == gameDimY = fadeToWhite
      | (x, y) `elem` titlePositions = Just $ ChangeGradually timeToFade (V3 0 0.9 0.9)
      | (x, y) `elem` menuOption = fadeToWhite
      | otherwise = Just $ ChangeImmediately (V3 0.1 0.1 0.1)
      where
        menuOption = startGamePositions ++ quitGamePositions

startGame :: Dict -> StageWire
startGame dict = (pure idleStage >>> delay fadeToGame >>> for timeToFade) --> (gameWire dict)
  where
    timeToFade = 2.0
    fadeToGame = mkStaticStageInput introInput
    introInput :: Int -> Int -> Maybe LetterInput
    introInput _ 1 = Nothing
    introInput x y
      | y == gameDimY = Nothing
      | otherwise = Just $ ChangeImmediately (V3 0.1 0.1 0.1)

gameOver :: Dict -> StageWire
gameOver dict = (pure idleStage >>> delay fadeToGameOver >>>
                 ((mkId >>> for timeToFade) --> L.quitWire GLFW.Key'Space)) -->
                (pure idleStage >>> delay fadeFromGameOver >>> mkEmpty) -->
                introSequence -->
                (gameMenu dict)
  where
    timeToFade = 2.0
    fadeToGameOver = mkStaticStageInput gameOverInput
    gameOverInput :: Int -> Int -> Maybe LetterInput
    gameOverInput _ 1 = Nothing
    gameOverInput x y
      | y == gameDimY = Nothing
      | (x, y) `elem` gameOverPositions = Just $ ChangeGradually timeToFade (V3 0.4 0.1 0.1)
      | otherwise = Just $ ChangeImmediately (V3 0.1 0.1 0.1)

    fadeFromGameOver = mkStaticStageInput gameOverOutput
    gameOverOutput _ _ = Just $ ChangeImmediately (V3 0.1 0.1 0.1)

numberToPositions :: Int -> Int -> [(Int, Int)]
numberToPositions x row =
  let ones = x `mod` 10
      tens = (x `mod` 100) `div` 10
      hunds = (x `mod` 1000) `div` 100
      thous = (x `mod` 10000) `div` 1000
  in (\(x,y) -> (40 - x, y)) <$> [(ones, row), (10 + tens, row), (20 + hunds, row), (30 + thous, row)]

numberFade = 0.2

setNumberRow :: Int -> Int -> V3 Float -> StageOutput -> StageOutput
setNumberRow x row color opt =
  let numberPos = numberToPositions x row
      allPos = [(x, row) | x <- [1..gameDimX]]
      f (x, y)
        | (x, y) `elem` numberPos = Just $ ChangeGradually numberFade color
        | otherwise = Just $ ChangeGradually numberFade (V3 1 1 1)
  in opt // ((\x -> (x, f x)) <$> allPos)

setScore :: Int -> StageOutput -> StageOutput
setScore x = setNumberRow x 1 (V3 0.9 0.1 0.1)

setTime :: Int -> StageOutput -> StageOutput
setTime x = setNumberRow x gameDimY (V3 0.2 0.6 0.2)


data BoardChar = BoardChar Char (Int, Int)

boardString :: [BoardChar] -> String
boardString = map (\(BoardChar c _) -> c)

data LState = LState {
  stateGen :: StdGen,
  timeTillLetter :: Float,
  letterWires :: [((Int, Int), L.GameWire () (Maybe LetterInput))],
  currentString :: [BoardChar],
  currentScore :: Int
}

initialState :: LState
initialState = LState {
  stateGen = unsafePerformIO getStdGen, -- !KLUDGE! ick
  timeTillLetter = 0.0,
  letterWires = [],
  currentString = [],
  currentScore = 0
}

pulseLetter :: Float -> L.GameWire () (Maybe LetterInput)
pulseLetter fadeTime =
  (pure Nothing >>> delay (Just $ ChangeGradually fadeTime (V3 0.2 0.9 0.9)) >>> for fadeTime) -->
  (pure Nothing >>> for (2.0 * fadeTime)) -->
  (pure Nothing >>> delay (Just $ ChangeGradually fadeTime (V3 0.1 0.1 0.1)) >>> for fadeTime)

type LetterGameWire = ((Int, Int), L.GameWire () (Maybe LetterInput))

genNewLetterGameWire :: StateT LState L.GameMonad LetterGameWire
genNewLetterGameWire = do
  lstate <- get
  let (x, g') = randomR (1, gameDimX) (stateGen lstate)
      (y, g'') = randomR (1, gameDimY) g'
      (fadeTime, g''') = randomR (0.5, 1.0) g''

  put $ lstate { stateGen = g''' }
  return ((x, y), pulseLetter fadeTime)

mkNewLetterGameWire :: StateT LState L.GameMonad ()
mkNewLetterGameWire = do
  lstate <- get
  w@(pos, _) <- genNewLetterGameWire
  case (any (\(x, _) -> x == pos) (letterWires lstate)) of
    True -> mkNewLetterGameWire
    False -> put $ lstate { letterWires = w : (letterWires lstate) }

runLetter :: L.TimeStep -> StageInput -> LetterGameWire -> StateT LState L.GameMonad [(Maybe LetterInput, LetterGameWire)]
runLetter ts ipt (pos, w) = do
  let LetterOutput c _ click _ = ipt ! pos
  case click of
    False -> do
      (result, w') <- lift $ stepWire w ts (Right ())
      case result of
        Left _ -> return []
        Right x -> return [(x, (pos, w'))]
    True -> do
      lstate <- get
      put $ lstate { currentString = (BoardChar c pos) : (currentString lstate) }
      return [(Just $ ChangeImmediately (V3 0.9 0.9 0.2), (pos, mkConst $ Right Nothing))]

debounceKey :: GLFW.Key -> L.GameMonad Bool
debounceKey key = do
  isPressed <- keyIsPressed key
  case isPressed of
    True -> releaseKey key
    False -> return ()
  return isPressed

runLetters :: Dict -> L.TimeStep -> StageInput ->
              StateT LState L.GameMonad [(Maybe LetterInput, LetterGameWire)]
runLetters dict ts ipt = do
  currentLetters <- letterWires <$> get
  letters <- concat <$> mapM (runLetter ts ipt) currentLetters
  space <- lift $ debounceKey GLFW.Key'Space
  lstate <- get
  case space of
    False -> return letters
    True -> do
      let str = boardString . nubBy ((==) `on` (\(BoardChar _ p) -> p)) . reverse . currentString $ lstate
      put $ lstate {
        currentString = [],
        letterWires = []}
      trace ("Checking string: " ++ str) $ return ()
      case (Set.member str dict) of
        True -> modify $ \st -> st { currentScore = (currentScore st) + (length str) }
        False -> return ()
      return $ (\(_, w) -> ((Just $ ChangeImmediately (V3 0.1 0.1 0.1)), w)) <$> letters

gameWire :: Dict -> StageWire
gameWire dict =
  ((runTime &&& runGame initialState runGameState) >>> setTimeWire) --> gameOver dict
  where
    startTime = 100.0

    runTime :: L.GameWire a Float
    runTime = timer startTime >>> W.when (>0)

    timer :: Float -> L.GameWire a Float
    timer start = mkSF $ \ts _ ->
      let newTime = start - (dtime ts)
      in (newTime, timer newTime)

    setTimeWire :: L.GameWire (Float, StageOutput) StageOutput
    setTimeWire = arr $ \(x, y) -> setTime (round x) y

    runGameState :: Wire L.TimeStep String (StateT LState L.GameMonad) StageInput StageOutput
    runGameState = mkGen $ \ts ipt -> do
      lstate <- get
      case (timeTillLetter lstate <= 0.0) of
        False -> put $ lstate { timeTillLetter = (timeTillLetter lstate) - (dtime ts) }
        True -> do
          let (x, newg) = randomR (0.05, 0.2) (stateGen lstate)
          put $ lstate { timeTillLetter = x }
          mkNewLetterGameWire

      letterFades <- runLetters dict ts ipt
      modify $ \st -> st { letterWires = snd <$> letterFades }
      let out = idleStage // ((\(ipt, (pos, _)) -> (pos, ipt)) <$> letterFades)
      score <- currentScore <$> get
      return $ (Right $ setScore score out, runGameState)

    runGame :: LState
               -> Wire L.TimeStep String (StateT LState L.GameMonad) StageInput StageOutput
               -> L.GameWire StageInput StageOutput
    runGame state gsw = mkGen $ \ts ipt -> do
      ((result, nextW), nextState) <- runStateT (stepWire gsw ts (Right ipt)) state
      return (result, runGame nextState nextW)
      

kNumMenuOptions :: Int
kNumMenuOptions = 2

kMenuPositions :: [(Int, Int)]
kMenuPositions = concat [startGamePositions, quitGamePositions]

gameMenu :: Dict -> StageWire
gameMenu dict = runWire (selection 0 >>> flickerSelection)
  where
    -- If enter is pressed then the wire inhibits forever with a value
    -- otherwise identity wire
    menuChoice :: L.GameWire Int Int
    menuChoice = mkGenN $ \x -> do
      enter <- keyIsPressed GLFW.Key'Enter
      case enter of
        False -> return (Right x, menuChoice)
        True ->
          case x of
            0 -> return (Left "Game", inhibit "Game")
            1 -> return (Left "Quit", inhibit "Quit")
            _ -> return (Left mempty, mkEmpty)

    selectionFeedback (f, x) = let y = f x in (y, y)
    selection x =
      ((keyDebounced GLFW.Key'Up >>> pure ((`mod` kNumMenuOptions) . (+1))) <|>
       (keyDebounced GLFW.Key'Down >>> pure ((`mod` kNumMenuOptions) . (+(kNumMenuOptions-1)))) <|>
       pure id) >>>
      (loop $ second (delay x) >>> (arr selectionFeedback)) >>>
      menuChoice

    flickerTime = 0.3

    pulseColor :: V3 Float -> [(Int, Int)] -> StageOutput
    pulseColor c positions = mkStaticStageInput findPos
      where
        findPos x y
          | (x, y) `elem` positions = Just $ ChangeGradually flickerTime c
          | (x, y) `elem` kMenuPositions = Just $ ChangeImmediately (V3 1 1 1)
          | otherwise = Nothing

    pulseYellow = pulseColor (V3 1 1 0)
    pulseWhite = pulseColor (V3 1 1 1)

    flickerSelection =
      ((W.when (== 0)) >>>
       ((pure idleStage >>> delay (pulseYellow startGamePositions) >>> for flickerTime) -->
        (pure idleStage >>> delay (pulseWhite startGamePositions) >>> for flickerTime))) -->
      ((W.when (== 1)) >>>
       ((pure idleStage >>> delay (pulseYellow quitGamePositions) >>> for flickerTime) -->
        (pure idleStage >>> delay (pulseWhite quitGamePositions) >>> for flickerTime))) -->
      flickerSelection

    runWire w = mkGen $ \ts ipt -> do
      (result, w') <- stepWire w ts (Right ipt)
      case result of
        Left "Game" -> return (Right idleStage, startGame dict)
        Left "Quit" -> return (Right idleStage, mkEmpty)
        Left x -> error $ "Unknown menu option: " ++ x
        Right x -> return (Right x, runWire w')

initialStage :: L.GameWire a StageOutput
initialStage =
  let startArray = array ((1, 1), (gameDimX, gameDimY))
                   [((x, y), Just $ ChangeGradually 1.0 (V3 1 1 0)) | x <- [1..gameDimX], y <- [1..gameDimY]]
  in
   (pure $ (\_ -> Nothing) <$> startArray) >>> delay startArray

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

initGame :: IO (L.Game ())
initGame = do
  putStr "Creating dictionary..."
  dict <- foldr Set.insert Set.empty . map trim . lines <$> readFile "dict.txt"
  putStrLn "Done."
  g <- mkGame (introSequence --> gameMenu dict)
  return $ L.Game {
    L.staticLights = [],
    L.staticGeometry = [],
    L.mainCamera = camera,
    L.dynamicLights = [],
    L.gameLogic = g }

main :: IO ()
main = do
  m <- L.makeWindow screenSizeX screenSizeY "Literate"
  g <- initGame
  case m of
    (Just win) -> L.run win () g
    Nothing -> return ()
  L.destroyWindow m
