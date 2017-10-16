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
randomList seed = take 3000000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
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
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs)
  -- cut both directions make 4 regions
  | w > half_Inital_Width && h > half_Initial_Height = (br_rest, ul_tags ++ ur_tags ++ bl_tags ++ br_tags)

  -- cut vertically make 2 regions
  | w > half_Inital_Width = (r_rest, left ++ right)

  -- cut horizontally only 2 regions
  | h > half_Initial_Height = (bottom_rest, top ++ bottom)
  
  -- cut both directions if legal split spots chosen 4 regions
  | w > 120 && h > 120 && hGood == True && vGood == True = (br_rest, ul_tags ++ ur_tags ++ bl_tags ++ br_tags)

  -- cut vertically 2 regions
  | w > 120 && vGood == True = (r_rest, left ++ right)

  -- cut horizontally 2 regions
  | h > 120 && hGood== True = (bottom_rest, top ++ bottom)

  | otherwise = 
      (rs, "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"Black\"" ++
       " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                         (show (round (s * 255))) ++ "," ++
                         (show (round (t * 255))) ++ ")\" />\n")
  where
    -- find half of initial width and height
    half_Inital_Width = div width 2
    half_Initial_Height = div height 2
    -- check if area should be split
    (hGood, hrs_rest) = (goodSplit w rs)
    (vGood, vrs_rest) = (goodSplit h hrs_rest)
    -- cut vertically and horizontally
    (vPos, hPos, rest) = twoSplit x y w h rs
    (ul_rest, ul_tags) = mondrian x y vPos hPos rest
    (ur_rest, ur_tags) = mondrian vPos y w hPos ul_rest
    (bl_rest, bl_tags) = mondrian x h vPos h ur_rest
    (br_rest, br_tags) = mondrian vPos hPos w h bl_rest
    -- cut vertically to get L and R halves
    (vSplitPt, vs_rest) = vSplit x w rs
    (l_rest, left) = mondrian x y vSplitPt h vs_rest
    (r_rest, right) = mondrian vSplitPt y w h l_rest
    -- cut horizontally to get T and B halves
    (hSplitPt, hs_rest) = hSplit y h rs 
    (top_rest, top) = mondrian x y w hPos hs_rest
    (bottom_rest, bottom) = mondrian x hPos w h top_rest
   

-- Returns a boolean to determine if region should be split
-- region = the w or h of region depending on split direction
-- r:rs a list of random floats
goodSplit :: Int -> [Float] -> (Bool, [Float])
goodSplit region (r:rs)
  | randInt < region = (True, rs)
  | otherwise = (False, rs)
  where 
    high = round (1.5 * (fromIntegral (region)) )
    randInt = randomInt 120 high r


-- Randomly selects a position between 33% and 67% of the region to split 
-- Parameters:
-- x coord
-- w width of region
-- r:rs random float list
-- Return: 
--  an int for the random split point and the rest of the random float list 
vSplit :: Int -> Int -> [Float] -> (Int, [Float])
vSplit x w (r:rs) =  (randomInt lowerBound (round (fromIntegral upperBound * 1.5)) r, rs)
  where
    lowerBound = round (fromIntegral (w) * 0.33)
    upperBound = round (fromIntegral (w) * 0.67)


-- Randomly selects a position between 33% and 67% of the region to split 
-- Parameters:
-- y coord
-- h height of region
-- r:rs random float list
-- Return: 
--  an int for the random split point and the rest of the random float list 
hSplit :: Int -> Int -> [Float] -> (Int, [Float])
hSplit y h (r:rs) = (randomInt lowerBound (round (fromIntegral upperBound * 1.5)) r, rs)
  where 
    lowerBound = round (fromIntegral (h) * 0.33)
    upperBound = round (fromIntegral (h) * 0.67)


-- split into 4 regions
-- Params:
-- x y coordinate values
-- w h width and height of region
-- r:rs list of random float values
-- Return:
-- [x, y, w, h] a list of tuples contianing x y w h for each region
-- [rs] list of remianing random floats
twoSplit :: Int -> Int -> Int -> Int -> [Float] -> (Int, Int, [Float])
twoSplit x y w h rs = (vPt, hPt, hp_rest)
  where 
    (vPt, vp_rest) = vSplit x w rs
    (hPt, hp_rest) = hSplit y h vp_rest
    


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

  