import SOE

main = test

makeCircle (x,y) r = ellipse (x-r, y-r) (x+r, y+r)

circles = reverse $ zipWith withColor colors bwcircles
  where bwcircles = [makeCircle center (25*i) | i <- [1..(length colors)]]
        colors    = [Red, Blue, Green, Cyan, Magenta, Yellow]
        center    = (350, 225)  

test = runGraphics $ do 
  w <- openWindow "Test" (300,300)
  drawInWindow w $ text (10,10) "Hello World" 
  sequence_ $ map (drawInWindow w) circles
  k <- getKeyChar w
  putStrLn $ "You pressed " ++ show k
  closeWindow w
