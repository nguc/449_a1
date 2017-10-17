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
-- Generate the tag for a rectangle with random color. 
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
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs)
        | w > half_Inital_Width && h > half_Initial_Height     = (gft_rest, ul_tags ++ ur_tags ++ bl_tags ++ br_tags)
        | w > half_Inital_Width                                = (v_rest, left ++ right)
        | h > half_Initial_Height                              = (h_rest, top ++ bottom)
        | w > 20 && h > 20 && vGood == True && hGood == True   = (gft2_rest, ul_t ++ ur_t ++ bl_t ++ br_t)
        | w > 20 && vGood == True                              = (v_rs, l_tag ++ r_tag)
        | h > 20 && hGood == True                              = (h_rs, t_tag ++ b_tag) 
        | otherwise                                            = 
                                                                 (rs, 
                                                                 "<rect x=" ++ (show x) ++ 
                                                                 " y=" ++ (show y) ++ 
                                                                 " width=" ++ (show w) ++ 
                                                                 " height=" ++ (show h) ++ 
                                                                 " stroke=\"Black\"" ++
                                                                 " fill =\"rgb(" ++ (colourArea x y r s t)  ++ ")\" />\n")
                                                                 {- " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                                                                 (show (round (s * 255))) ++ "," ++
                                                                 (show (round (t * 255))) ++ ")\" />\n")  -}
                                                                                                                
        where
                half_Inital_Width       = div width 2
                half_Initial_Height     = div height 2
                (gft_rest, ul_tags, ur_tags, bl_tags, br_tags) = genFourTags x y w h rs
                (v_rest, left, right)   = genLRTags x y w h rs
                (h_rest, top, bottom)   = genTBTags x y w h rs 
                -- random check if area should be split
                (vGood, vrs_rest)       = (isgoodSplit h rs)
                (hGood, hrs_rest)       = (isgoodSplit w vrs_rest)
                (gft2_rest, ul_t, ur_t, bl_t, br_t) = genFourTags x y w h hrs_rest
                (v_rs, l_tag, r_tag)    = genLRTags x y w h hrs_rest
                (h_rs, t_tag, b_tag)    = genTBTags x y w h hrs_rest
        

-- Returns 4 tags when splitting in both directions as well as the list of remaining random floats
genFourTags :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String, String, String, String)
genFourTags x y w h rs = (br_rs, ul_tag, ur_tag, bl_tag, br_tag)
        where
                (vPos, hPos, rest) = twoSplitPositions x y w h rs
                (ul_rs, ul_tag)    = mondrian x y (vPos-x) (hPos-y) rest
                (ur_rs, ur_tag)    = mondrian vPos y ((x+w)-vPos) (hPos-y) ul_rs
                (bl_rs, bl_tag)    = mondrian x hPos (vPos-x) ((y+h)-hPos) ur_rs
                (br_rs, br_tag)    = mondrian vPos hPos ((x+w)-vPos) ((y+h)-hPos) bl_rs

-- returns 2 tags for the left and right rectangle when splitting vertically
genLRTags ::  Int -> Int -> Int -> Int -> [Float] -> ([Float], String, String)
genLRTags x y w h rs = (r_rest, left, right)
        where
                (splitPtX, vs_rest)  = splitPosition x w rs
                (l_rest, left)       = mondrian x y (splitPtX-x) h vs_rest
                (r_rest, right)      = mondrian splitPtX y ((x+w)-splitPtX) h l_rest   

 -- returns 2 tags for the top and bottom rectangles when splitting horizontally 
genTBTags ::  Int -> Int -> Int -> Int -> [Float] -> ([Float], String, String)
genTBTags x y w h rs = (b_rest, top, bottom)
        where
                (splitPtY, hs_rest) = splitPosition y h rs
                (t_rest, top)       = mondrian x y w (splitPtY-y) hs_rest
                (b_rest, bottom)    = mondrian x splitPtY w ((y+h)-splitPtY) t_rest
                

 
-- returns 2 randomly chosen split points, one for the vertical and one for the horizontal, that will be used
-- to split a rectangle into 4 quadrants.
twoSplitPositions :: Int -> Int -> Int -> Int -> [Float] -> (Int, Int, [Float])
twoSplitPositions x y w h rs = (vPt, hPt, h_list)
        where 
                (vPt, v_list) = splitPosition x w rs
                (hPt, h_list) = splitPosition y h v_list

-- returns a randomly chosen split point for dividing a region into 2 parts
splitPosition :: Int -> Int -> [Float] -> (Int, [Float])
splitPosition iCoord size (r:rs) =  (randomInt lowerBound upperBound r, rs)
        where
                third      = round (fromIntegral (size) * 0.20)
                lowerBound = iCoord + third
                upperBound = iCoord + (2 * third)

-- returns a boolean to determine if a rectangle should be split or not
isgoodSplit :: Int -> [Float] -> (Bool, [Float])
isgoodSplit region (r:rs)
        | randInt < region = (True, rs)
        | otherwise        = (False, rs)
        where 
                high       = round (1.5 * (fromIntegral (region)) )
                low        = 20
                randInt    = randomInt low high r

-- colours the area by quadrant. Starting from the top left and going clockwise the quadrants will be
-- coloured in different intensities of red, green, yellow, purple
colourArea :: Int -> Int -> Float -> Float -> Float -> String
colourArea x y r s t
        | topLeft     = show (round (r*255)) ++ ",0,0"
        | topRight    = "0," ++ show (round (r*255)) ++ ",0"
        | bottomLeft   = "0,0," ++ show (round (r*255))
        | bottomRight = "255," ++ show (round ( (yellow s) * 255)) ++ ",0"
        where
                halfW       = div width 2
                halfH       = div height 2
                topLeft     = x <= halfW && y <= halfH
                topRight    = x >  halfW && y <= halfH
                bottomLeft  = x <= halfW && y >  halfH
                bottomRight = x >  halfW && y >  halfH
               

-- determines if the value of s is the correct range to produe a yellow hue. 
-- If not generate a new value for s that will be in range
yellow :: Float -> Float
yellow s 
        | (255 - (s * 255) ) > 95 = fromIntegral (div 255 (randomInt (255 - 95) 255 s) )
        | otherwise      =  s
        
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

  writeFile "mondrianBonus.html" (prefix ++ image ++ suffix)

  