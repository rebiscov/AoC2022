import qualified Data.Map as Map
import Data.Map ( Map, insertWith, foldrWithKey )
import Data.List ( map, filter, minimum )
import Data.List.Split
import Data.Set
import Data.Either
import Data.Either.Combinators ( mapBoth )

data Blob = File String Int | Dir String
  deriving (Show, Eq, Ord)

type AbsPath = [ String ]

type Filesystem = Map AbsPath (Set Blob)
data Command = CD String | LS
  deriving (Show)


parseCommand :: String -> Command
parseCommand l = if (e !! 1) == "ls" then LS else CD (e !! 2) where e = splitOn " " l

parseRes :: String -> Blob
parseRes l =
  if head tokens == "dir" then Dir (tokens !! 1)
  else File (tokens !! 1) ((read $ head tokens) :: Int) where tokens = splitOn " " l

parseLine l =
  if head l == '$' then Left $ parseCommand l
  else Right $ parseRes l

updateFS :: AbsPath -> Set Blob -> Filesystem -> Filesystem
updateFS = Map.insertWith Data.Set.union

updatePath :: AbsPath -> Command -> AbsPath
updatePath s (CD "..") = tail s
updatePath s (CD dir) = dir:s
updatePath s _ = s


k :: AbsPath -> Filesystem -> [String] -> Either AbsPath Filesystem -> Filesystem
k _ fs lines (Left newPath) = buildFS newPath fs lines
k currentPath _ lines (Right fs) = buildFS currentPath fs lines
  
buildFS :: AbsPath -> Filesystem -> [String] -> Filesystem
buildFS currentPath fs [] = fs
buildFS currentPath fs (line:mylines) =
  k currentPath fs mylines $
  mapBoth (updatePath currentPath) (\blob -> updateFS currentPath (Data.Set.singleton blob) fs) $ parseLine line

sizeBlob :: AbsPath -> Filesystem -> Blob -> Int
sizeBlob path fs (Dir name) = sizeDir (name:path) fs
sizeBlob path fs (File name o) = o

sizeDir :: AbsPath -> Filesystem -> Int
sizeDir path fs = sum $ Data.Set.map (sizeBlob path fs) $ Map.findWithDefault Data.Set.empty path fs

partOne = do
  mylines <- lines <$> getContents
  let fs = buildFS []  Map.empty (tail mylines)
  print $ foldrWithKey (\path _ sum -> if sizeDir path fs > 100000 then sum else sum + sizeDir path fs) 0 fs

partTwo = do
  mylines <- lines <$> getContents
  let fs = buildFS []  Map.empty (tail mylines)
  let toFree = 30000000 - (70000000 - sizeDir [] fs)
  let dirSizes = Data.List.filter  (>=toFree) $ Data.List.map (\(path, _) -> sizeDir path fs) $ Map.toList fs

  print $ Data.List.minimum dirSizes

main = partTwo
