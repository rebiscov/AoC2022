import Data.List.Split
import GHC.Utils.Misc ( last2 )

readInt :: String -> Integer
readInt = read

parseCouple s = last2 $ map readInt $ splitOn "-" s
parseLine l = last2 $ map parseCouple $ splitOn "," l

subset (i1, i2) (j1, j2) = i1 <= j1 && j2 <= i2
oneIsSubset t1 t2 = subset t1 t2 || subset t2 t1

overlap (i1, i2) (j1, j2) = not (i2 < j1 || j2 < i1)

part1 () = do
  contents <- getContents
  let parsedLines = map parseLine $ lines contents
  let res = length $ filter (uncurry oneIsSubset) parsedLines
  print $ show res

part2 () = do
  parsedLines <- map parseLine . lines <$> getContents
  let res = length $ filter (uncurry overlap) parsedLines
  print $ show res

main = part2 ()
