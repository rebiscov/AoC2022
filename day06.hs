areAllDifferent [] = True
areAllDifferent (x:xs) = notElem x xs && areAllDifferent xs


indexOfNDiff l n i =
  if areAllDifferent $ take n l then i
  else indexOfNDiff (tail l) n (i+1)

partOne = do
  s <- getContents
  print $ 4 + indexOfNDiff s 4 0

partTwo = do
  s <- getContents
  print $ 14 + indexOfNDiff s 14 0

main = partTwo
