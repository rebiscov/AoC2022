import Data.Char ( ord )
import Data.List.Split ( chunksOf )
import Data.Maybe ( fromJust )

charToPrio c =
  if ord c >= ord 'a' then ord c - ord 'a' + 1
  else ord c - ord 'A' + 27

inter [] _ = Nothing
inter (i:s1) s2 = if elem i s2 then Just i else inter s1 s2

inter2 [] _ _ = Nothing
inter2 (i:s1) s2 s3 = if elem i s2 && elem i s3 then Just i else inter2 s1 s2 s3

splitInTwo l = splitAt (div (length l) 2)  l

valueOfItem line =
  let (l1, l2) = splitInTwo line in
  case inter l1 l2 of
    Just c -> charToPrio c
    Nothing -> 0

part1 () = do
  contents <- getContents
  let myLines = lines contents
  putStrLn $ show $ foldr (\l sum -> valueOfItem l + sum) 0 myLines

part2 () = do
  contents <- getContents
  let myLines = lines contents
  let chunksOf3 = chunksOf 3 myLines
  let commonItems = map (\l -> inter2 (l !! 0) (l !! 1) (l !! 2)) chunksOf3
  let res = foldr (\mi sum -> sum + (charToPrio $ fromJust mi)) 0 commonItems
  putStrLn $ show res

main = part2 ()

  
