import Data.List
import Data.List.Split
import GHC.Utils.Misc ( last2, liftFst )

data Hand = Rock | Paper | Scissors
eq Rock Rock = True
eq Paper Paper = True
eq Scissors Scissors = True
eq _ _ = False

instance Eq Hand where
  (==) = eq

showHand Rock = "Rock"
showHand Paper = "Paper"
showHand Scissors = "Scissors"

instance Show Hand where
  show = showHand 

suc Paper = Scissors 
suc Scissors = Rock
suc Rock = Paper

losesAgainst :: Hand -> Hand -> Bool
losesAgainst h1 h2 = suc h1 == h2

scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

computeScoreFst p1 p2 =
  (
    if p1 `losesAgainst` p2 then 0 
    else if p2 `losesAgainst` p1 then 6
    else 3)
  + scoreHand p1

stringToHand "A" = Rock
stringToHand "B" = Paper
stringToHand "C" = Scissors
stringToHand "X" = Rock
stringToHand "Y" = Paper
stringToHand "Z" = Scissors

stratToHands p "X" = (p, suc $ suc p)
stratToHands p "Y" = (p, p)
stratToHands p "Z" = (p, suc p)

parseLine l = last2 $ map stringToHand $ splitOn " " l
parseLineBis l = uncurry stratToHands $ liftFst stringToHand $ last2 $ splitOn " " l

main = do
  contents <- getContents
  let myLines = map parseLineBis $ lines contents 
  let scores = map (uncurry $ flip computeScoreFst) myLines
  print $ show $ sum scores
