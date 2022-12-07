{-# LANGUAGE TypeSynonymInstances #-}
import Data.List.Split
import Data.List
import Data.Foldable

instance Semigroup Integer where
    (<>) = (+)

instance Monoid Integer where
    mempty  = 0
    mappend = (+)

mysum = Data.Foldable.fold

mymax::NewInteger -> NewInteger -> NewInteger
mymax = Data.Foldable.fold

main = do
  contents <- getContents
  let lineList = splitOn "\n" contents
  let elfFood = splitWhen (== "") lineList
  let elfFoodAsInt = map (map (\s -> read s :: Integer)) elfFood
  let elfFoodSum = reverse $ sort $ map mysum elfFoodAsInt
  putStrLn (show ((elfFoodSum !! 0) + (elfFoodSum !! 1) + (elfFoodSum !! 2)))
