module MyAnimation where

import Animation

picture :: Animation
picture = translate (ever (width/2, height/2)) (combine (circles 10))

-- Width and height of the .svg
width :: Double
width = 800

height :: Double
height = 600

-- Makes a circle whos colour can change and the scale changes over time to oscillate
makeCircle :: Varying Colour -> Double -> Animation
makeCircle col sc =
 translate (ever (width/2, height/2))
 (scale (repeatSmooth (0,0) [(1 ,(0, 0)), (2, (sc/4, sc/4)), (3, (0, 0)) ])
 (withBorder (col) (ever 25)
 (withoutPaint (circle (ever 300)))))

-- Makes more than one circle, the colour of the circles change every second
circles :: Int -> [Animation]
circles secs = [makeCircle col (fromIntegral n) | col <- [cycleSteps 1 [blue, yellow, red, green, purple]], n <- [secs, secs-1 .. 1] ]

pic :: Animation
pic = combine (circles 10)

test :: IO ()
test = writeFile "test.svg" (svg 800 600 pic)
