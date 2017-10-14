-- Website:
-- pages.cpsc.ucalgary.ca/~hartja

-- Koch Curve
-- divide each line into 1/3

-- Helper functions to make the Koch line

add::(Float, Float)->(Float, Float)->(Float, Float)
add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

sub::(Float, Float)->(Float, Float)->(Float, Float)
sub (x1, y1) (x2, y2) = (x1-x2, y1-y2)

scale::(Float, Float)->Float->(Float, Float)
scale (x,y) s = (s*x, s*y)

perp::(Float, Float)->(Float, Float)
perp (x, y) = (-y, x)

neg::(Float, Float)->(Float, Float)
neg p = scale p (-1)


-- this is what is inside of the line tag
-- <line
--   x1 - first x coord
--   y1 - first y coord
--   x2 - second x coord
--   y2 - second y coord
--   style
-- />

-- int is the amount of times this function will repeat
kochLine::(Float, Float)->(Float, Float)->Int->String
kochLine (x1, y1) (x2, y2) 0            = "<line x1=\"" ++ x1_s ++ "\" y1=\"" ++ y1_s ++ 
                                        "\" x2=\"" ++ x2_s ++ "\" y2=\"" ++ y2_s ++
                                        "\" style=\"stroke:rgb(255,0,0);stroke-width:2\" />"
        where
                x1_s = show (round x1)
                x2_s = show (round x2)
                y1_s = show (round y1)
                y2_s = show (round y2)

-- this defines what your variables mean
--kochLine (x1, y1) (x2, y2) n = (kochLine p1 p2 (n-1)) ++ (kochLine p2 p3 (n-1)) ++ (kochLine p3 p4 (n-1)) ++ (kochLine p4 p5 (n-1)) ++ (kochLine p5 p6 (n-1))

kochLine (x1, y1) (x2, y2) n = (kochLine p1 p2 (n-1)) ++ (kochLine p2 p3 (n-1)) ++ (kochLine p3 p4 (n-1)) ++ (kochLine p4 p5 (n-1))

        where
                line = sub (x2, y2) (x1, y1)
                line_p = perp line
                p1 = (x1, y1)
                p2 = add p1 (scale line (1.0/3.0))
                p3 = add p2 (scale (add (scale line (cos 60)) (scale line_p (sin 60)) )(1.0/3.0))
                p4 = add p3 (scale (add (scale line (cos (-120))) (scale line_p (sin (-120))) )(1.0/3.0))
                p5 = add p4 (scale (neg line_p) (1.0/3.0))


               -- p3 = add p2 (scale line_p (1.0/3.0))
               -- p4 = add p3 (scale line (1.0/3.0))
               -- p5 = add p4 (scale (neg line_p) (1.0/3.0))
               -- p6 = add p5 (scale line (1.0/3.0))

-- main just writes the tags to draw the lines
main::IO()
main = do
        let prefix = "<html><svg width=\"512\" height=\"512\">"
        let suffix = "</svg></html>"
        let image_tags = kochLine (1, 1) (500, 500) 2  -- change the last param (int) to add more fractals

        writeFile "koch.html" (prefix ++ image_tags ++ suffix)

