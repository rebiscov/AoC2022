import Data.List.Split
import Data.Bifunctor

type Move = (Int, Int, Int)

type Stack = [Char]
type Stacks = [[Char]]


takeFromStacks n from stacks =
  let (stacksPref, s:stacksSuff) = splitAt from stacks in
    let (s1, s2) = splitAt n s in
      (s1, stacksPref ++ s2:stacksSuff)

putInStacks elems to stacks =
  let (stacksPref, s:stacksSuff) = splitAt to stacks in
      stacksPref ++ (elems ++ s):stacksSuff

move (n, from, to) stacks =
  let (s, stacks') = takeFromStacks n from stacks in
    putInStacks (reverse s) to stacks'

moveBis (n, from, to) stacks =
  let (s, stacks') = takeFromStacks n from stacks in
    putInStacks s to stacks'

parseStackLine s stacks = 
  let chunks = chunksOf 4 s in
    zipWith (\head stack -> if (head !! 1) == ' ' then stack else head !! 1:stack) chunks stacks

parseStacks myLines = foldr parseStackLine (replicate (succ $ div (length $ head myLines) 4)  []) myLines

parseMoveLine l =
  let line = splitOn " " l in
    (read $ line !! 1, read (line !! 3) - 1, read (line !! 5) - 1)::Move

splitStacksMoves s =
  let myLines = lines s in
    let (stacks, _:_:moves) = span (\l -> l !! 1 /= '1') myLines in
      (stacks, moves)

partOne = do
  (stacks, moves) <- bimap parseStacks (map parseMoveLine) . splitStacksMoves <$> getContents 
  let stacks' = foldr move stacks (reverse moves)
  print $  map head stacks'

partTwo = do
  (stacks, moves) <- bimap parseStacks (map parseMoveLine) . splitStacksMoves <$> getContents 
  let stacks' = foldr moveBis stacks (reverse moves)
  print $  map head stacks'

main = partTwo
