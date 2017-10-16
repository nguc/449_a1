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
randomList seed = take 6000000 (rl_helper (mkStdGen seed))

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
      {- " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                         (show (round (s * 255))) ++ "," ++
                         (show (round (t * 255))) ++ ")\" />\n") 
      -}
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs)
  -- cut both directions make 4 regions
  | w > half_Inital_Width && h > half_Initial_Height = (br_rest, ul_tags ++ ur_tags ++ bl_tags ++ br_tags)
  -- cut vertically make 2 regions
  | w > half_Inital_Width = (r_rest, left ++ right)
  -- cut horizontally only 2 regions
  | h > half_Initial_Height = (b_rest, top ++ bottom)
  -- cut both directions if legal split spots chosen 4 regions
  | w > 120 && h > 120 && vGood == True && hGood == True = (br_rest2, ul_tags2 ++ ur_tags2 ++ bl_tags2 ++ br_tags2)
  -- cut vertically 2 regions
  | w > 120 && vGood == True = (r_rest, left2 ++ right2)
  -- cut horizontally 2 regions
  | h > 120 && hGood == True = (b_rest, top2 ++ bottom2)
  | otherwise = 
      (rs, "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"Black\"" ++
       " fill =\"rgb(" ++ (colour r s t)  ++ ")\" />\n")

  where
    -- find half of initial width and height
    half_Inital_Width = div width 2
    half_Initial_Height = div height 2
    -- code for first 3 checks
    -- cut vertically and horizontally
    (vPos, hPos, rest) = twoSplit x y w h rs
    (ul_rest, ul_tags) = mondrian x y (vPos-x) (hPos-y) rest
    (ur_rest, ur_tags) = mondrian vPos y ((x+w)-vPos) (hPos-y) ul_rest
    (bl_rest, bl_tags) = mondrian x hPos (vPos-x) ((y+h)-hPos) ur_rest
    (br_rest, br_tags) = mondrian vPos hPos ((x+w)-vPos) ((y+h)-hPos) bl_rest
    -- cut vertically to get L and R halves
    (sPointV, vs_rest) = splitPosition x w rs
    (l_rest, left) = mondrian x y (sPointV-x) h vs_rest
    (r_rest, right) = mondrian sPointV y ((x+w)-sPointV) h l_rest
    -- cut horizontally to get T and B halves
    (sPointH, hs_rest) = splitPosition y h rs
    (t_rest, top) = mondrian x y w (sPointH-h) hs_rest
    (b_rest, bottom) = mondrian x sPointH w ((y+h)-sPointH) t_rest
    -- code for last 3 checks
    -- check if area should be split
    (vGood, vrs_rest) = (goodSplit h rs)
    (hGood, hrs_rest) = (goodSplit w vrs_rest)
     -- cut vertically and horizontally
    (vPos2, hPos2, rest2) = twoSplit x y w h hrs_rest
    (ul_rest2, ul_tags2) = mondrian x y (vPos2-x) (hPos2-y) rest2
    (ur_rest2, ur_tags2) = mondrian vPos2 y ((x+w)-vPos2) (hPos2-y) ul_rest2
    (bl_rest2, bl_tags2) = mondrian x hPos2 (vPos2-x) ((y+h)-hPos2) ur_rest2
    (br_rest2, br_tags2) = mondrian vPos2 hPos2 ((x+w)-vPos2) ((y+h)-hPos2) bl_rest2
    -- cut vertically to get L and R halves
    (sPointV2, vs_rest2) = splitPosition x w hrs_rest
    (l_rest2, left2) = mondrian x y (sPointV2-x) h vs_rest2
    (r_rest2, right2) = mondrian sPointV2 y ((x+w)-sPointV2) h l_rest2
    -- cut horizontally to get T and B halves
    (sPointH2, hs_rest2) = splitPosition y h hrs_rest
    (t_rest2, top2) = mondrian x y w (sPointH2-h) hs_rest2
    (b_rest2, bottom2) = mondrian x sPointH2 w ((y+h)-sPointH2) t_rest2
    

-- Returns a boolean to determine if region should be split
-- region = the w or h of region depending on split direction
-- r:rs a list of random floats
goodSplit :: Int -> [Float] -> (Bool, [Float])
goodSplit region (r:rs)
  | randInt < region = (True, rs)
  | otherwise = (False, rs)
  where 
    high = round (1.5 * (fromIntegral (region)) )
    low = 120
    randInt = randomInt low high r


-- Randomly selects a position between 33% and 67% of the region to split 
-- Parameters:
-- x coord
-- w width of region
-- r:rs random float list
-- Return: 
--  an int for the random split point and the rest of the random float list 
splitPosition :: Int -> Int -> [Float] -> (Int, [Float])
splitPosition iCoord size (r:rs) =  (randomInt lowerBound upperBound r, rs)
  where
    third = round (fromIntegral (size) * 0.33)
    lowerBound = iCoord + third
    upperBound = (iCoord + size) - third

-- split into 4 regions
-- Params:
-- x y coordinate values
-- w h width and height of region
-- r:rs list of random float values
-- Return:
-- [x, y, w, h] a list of tuples contianing x y w h for each region
-- [rs] list of remianing random floats  
twoSplit :: Int -> Int -> Int -> Int -> [Float] -> (Int, Int, [Float])
twoSplit x y w h rs = (vPt, hPt, h_list)
  where 
    (vPt, v_list) = splitPosition x w rs
    (hPt, h_list) = splitPosition y h v_list

colour :: Float -> Float -> Float -> String
colour r g b
  | r > g && r > b = show (round (r*255)) ++ ",0,0"
  | g > r && g > b = show (round (g*255)) ++ "," ++show (round (g*255)) ++ ",0"
  | b > r && b > g = "0,0," ++ show (round (b*255))
  | otherwise = "0,0,0"



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

  