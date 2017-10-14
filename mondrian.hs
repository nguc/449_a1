--
-- Starting code for CPSC 449 Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low (120) and high (width * 1.5) from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
-- Generate the tag for a rectangle with random color.  Replace the 
-- implementation of this function so that it generates all of the tags
-- needed for a piece of random Mondrian art.
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
{-| mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs) = 
  (rs, "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"None\"" ++
       " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                         (show (round (s * 255))) ++ "," ++
                         (show (round (t * 255))) ++ ")\" />\n")
  -}

mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs)
  -- cut both directions make 4 regions
  | w > half_Inital_Width && h > half_Initial_Height = 
      (br_rest, ul ++ ur ++ bl ++ br)

  -- cut vertically make 2 regions
  -- | w > half_Inital_Width

  -- cut horizontally only 2 regions
  -- | h > half_Initial_Height
  
  -- cut both directions if legal split spots chosen 4 regions
  -- | w > 120 && h > 120 && hGood && vGood

  -- cut vertically 2 regions
  -- | w > 120 && vGood

  -- cut horizontally 2 regions
  -- | h > 120 && hGood

  | otherwise = 
      (rs, "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"None\"" ++
       " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                         (show (round (s * 255))) ++ "," ++
                         (show (round (t * 255))) ++ ")\" />\n")
  where
    half_Inital_Width = div width 2
    half_Initial_Height = div height 2
    (hGood, hrs_rest) = (goodSplit w rs)
    (vGood, vrs_rest) = (goodSplit h hrs_rest)
    (vSplitPt, vsp_rest) = (vSplit x w rs)
    (hSplitPt, hsp_rest) = (hSplit y h vsp_rest)
    left_w = vSplitPt - x
    right_w = w - vSplitPt
    top_h = hSplitPt - h
    bottom_h = h - hSplitPt
    (ul_rest, ul) = mondrian x y left_w top_h rs
    (ur_rest, ur) = mondrian vSplitPt y right_w top_h ul_rest
    (bl_rest, bl) = mondrian x hSplitPt left_w bottom_h ur_rest
    (br_rest, br) = mondrian vSplitPt hSplitPt right_w bottom_h bl_rest
   



------------------------Added code ------------------------------------
-- Returns a boolean to determine if region should be split
-- region = the w or h of region depending on split direction
-- r:rs a list of random floats
goodSplit :: Int -> [Float] -> (Bool, [Float])
goodSplit region (r:rs)
  | randomInt 120 (fromIntegral region * 1.5) r < region = (True, rs)
  | otherwise = (False, rs)



-- Randomly selects a position to split the region
-- Parameters:
-- x coord
-- w width of region
-- r:rs random float list
-- Return: 
--  an int for the random split point and the rest of the random float list 
vSplit :: Int -> Int -> [Float] -> (Int, [Float])
vSplit x w (r:rs)
  | randomPosition > lowerBound && randomPosition < upperBound = (randomPosition, rs)
  | otherwise = vSplit x w rs -- recursively calls until a valid position is found
  where 
    randomPosition = randomInt 120 (fromIntegral(w) * 1.5) r
    regionLength = w - x
    lowerBound = fromIntegral regionLength * 0.33
    upperBound = fromIntegral regionLength * 0.67

hSplit :: Int -> Int -> [Float] -> (Int, [Float])
hSplit y h (r:rs)
  | randomPosition > lowerBound && randomPosition < upperBound = (randomPosition, rs)
  | otherwise = hSplit y h rs -- recursively calls until a valid position is found
  where
    high = toInteger (fromIntegral h * 1.5)
    randomPosition = randomInt 120 high r
    regionLength = h - y
    lowerBound = fromIntegral regionLength * 0.33
    upperBound = fromIntegral regionLength * 0.67



--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)

  