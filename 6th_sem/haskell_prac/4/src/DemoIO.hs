module DemoIO where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment
import Text.Read
--------------
-- Data types.
--------------

-- Config for colors.
data ColorConfig = ColorConfig
  { color1 :: Color
  , color2 :: Color
  }

-- General application state.
data AppState = AppState
  { number :: Int -- Random number generator.
  , randomGen :: StdGen -- Current number.
  , colors :: ColorConfig -- Colors config.
  }

-- * 1 task
-- Essential configs given on the start of the app.
data AppConfig = AppConfig
  { fileName :: String
  , initNumb :: Int
  }
-- * 1 task
-------------
-- Constants.
-------------

-- Random numbers range.
numbersRange :: (Int, Int)
numbersRange = (-10, 10)

-- Game display mode.
display :: Display
display = FullScreen

-- Background color.
bgColor :: Color
bgColor = greyN 0.05

-- Simulation steps per second.
fps :: Int
fps = 60

-- Text shift on screen.
textShift :: Float
textShift = 250

------------------
-- Pure functions.
------------------

-- Parse config from string.
-- Config format: 2 lines, one color per line.
parseConfig :: String -> Maybe ColorConfig
parseConfig str = case map findColor (lines str) of
  [Just c1, Just c2] -> Just $ ColorConfig c1 c2
  _ -> Nothing
  where
    findColor :: String -> Maybe Color
    findColor s = lookup s colorMap
    colorMap = zip names colors
    colors = [red, green, blue, white, yellow, orange, violet]
    names = ["red", "green", "blue", "white", "yellow", "orange", "violet"]

-- * 1 task
-- Parse arguments from list of String.
-- AppConfig must contain String as a first argument and Int as second.
parseArgs :: [String] -> Either AppConfig String
parseArgs [a, b] =
  case readMaybe b :: Maybe Int of
    Nothing -> Right "Wrong second argument"
    Just numb -> Left $ AppConfig a numb
parseArgs _ = Right "Wrong arguments"
-- * 1 task

-- Draw a picture: two numbers of different colors defined in config.
drawApp :: AppState -> Picture
drawApp (AppState n _ (ColorConfig c1 c2)) = Pictures [pic1, pic2]
  where
    pic1 = Color c1 $ Translate (-textShift) 0 txt
    pic2 = Color c2 $ Translate textShift 0 txt
    txt = Text (show n)

-- Handle events.
handleEvent :: Event -> AppState -> AppState

-- * 2 task
-- Replace current value with 0 when clicked any mouse button.
handleEvent (EventKey (MouseButton _) Down _ _) state =
  state { number = 0 }
-- * 2 task

-- Increase number when UP is pressed.
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state =
  state { number = number state + 1 }
-- Decrease number when DOWN is pressed.
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state =
  state { number = number state - 1 }
-- Generate new random number when Space is pressed.
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) (AppState _ r c) =
  -- Get new random number and generator.
  let (newn, newr) = randomR numbersRange r
  -- Update BOTH number AND generator.
  in AppState newn newr c
-- Ignore all other events.
handleEvent _ state = state

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ x = x


------------------------------
-- Main function for this app.
------------------------------

-- Run game. This is the ONLY unpure function.
run :: IO ()
run = do
  -- Load command line arguments.
  args <- getArgs
  case parseArgs args of
    Right a -> putStrLn a
    Left args -> do
      str <- readFile (fileName args)
      -- Try to parse config.
      case parseConfig str of
        Nothing -> putStrLn "Wrong config"
        Just cfg -> do
          -- Get new random number generator (unpure action).
          rndGen <- newStdGen
          -- Run application.
          play display bgColor fps (AppState (initNumb args) rndGen cfg) drawApp
            handleEvent updateApp


