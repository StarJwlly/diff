import System.Environment
import Data.Text hiding (head, tail)
import Data.Char
import Data.List
import System.IO
import Data.Ratio

main = do
  (path : _) <- getArgs
  h <- openFile path ReadMode
  hSetEncoding h utf8
  contents <- hGetContents h
  let timingsLine = snd $ Prelude.break ("[TimingPoints]"==) $ Data.List.lines contents
  let timingPoints = getTimingPoints timingsLine
  let notesLine = tail $ snd $ Prelude.break ("[HitObjects]"==) timingsLine
  --putStrLn $ show $ getTimingPoints timingsLine
  --putStrLn $ show $ readHitObjects notesLine 10
  putStrLn $ show $ getDistances (readHitObjects notesLine 10) $ Data.List.replicate 10 (Nothing, Nothing)
  
  

splitOnDelimiter :: String -> String -> [String]
splitOnDelimiter s d = Data.List.map (unpack) $ splitOn (pack d) $ pack s


data TimingPoint = TimingPoint {offset :: Int, beatLen :: Double, inherited :: Bool} deriving (Show)


toTimingPoint :: String -> TimingPoint
toTimingPoint s = TimingPoint (read (x !! 0)) (read (x !! 1)) ((x !! 6) == "0")
                    where x = splitOnDelimiter s ","

getTimingPoints :: [String] -> [TimingPoint]
getTimingPoints (l:ls) = Data.List.filter (\x -> not (inherited x)) $ Data.List.map toTimingPoint $ fst $ Prelude.break (""==) ls
                           

data Note = HitNote | HoldNote | ReleaseNote deriving (Show, Eq)
data HitObject = HitCircle {timeH :: Int, columnH :: Int} | LongNote {timeH :: Int, columnH :: Int, releaseTime :: Int} deriving (Show)

data Chord = Chord {time :: Int, notes :: [Maybe Note]} deriving (Show)


toHitObject :: Int -> String -> HitObject
toHitObject keyCount str | t == "128" = LongNote (read (n !! 2)) c (read $ head $ splitOnDelimiter (n !! 5) ":")
                         | otherwise  = HitCircle (read (n !! 2)) c
                           where n = splitOnDelimiter str ","
                                 t = n !! 3
                                 c = floor (fromIntegral ((read (n !! 0)) * keyCount) / 512)
                         

readHitObjects :: [String] -> Int -> [Chord]
readHitObjects strs keyCount = let hitObjects = Data.List.map (toHitObject keyCount) strs in readHitObjectsLoop hitObjects (Prelude.replicate keyCount Nothing)
  
  
readHitObjectsLoop :: [HitObject] -> [Maybe Int] -> [Chord]
readHitObjectsLoop [] releases = 
  if Data.List.null trs
    then []
    else let c = toChord [] releases tr in (fst c) : readHitObjectsLoop [] (snd c)
      where tr = Data.List.minimum $ Data.List.map (\(Just v) -> v) trs
            trs = Data.List.filter (\x -> x /= Nothing) releases
readHitObjectsLoop hitObjects releases = 
  if (not $ Data.List.null trs) && tr < t
    then let c = toChord [] releases tr in (fst c) : readHitObjectsLoop hitObjects (snd c)
    else let c = toChord (fst same) releases t in (fst c) : readHitObjectsLoop (snd same) (snd c)
      where same = Prelude.break (\x -> (timeH x) /= t) hitObjects
            t = timeH $ head hitObjects
            tr = Data.List.minimum $ Data.List.map (\(Just v) -> v) trs
            trs = Data.List.filter (\x -> x /= Nothing) releases


toChord :: [HitObject] -> [Maybe Int] -> Int -> (Chord, [Maybe Int])
toChord hitObjects releases tim = let r = unzip result in (Chord tim (fst r), snd r)
  where substitute a i n = let s = Prelude.splitAt i a in (fst s) ++ (n : (tail $ snd s))
        isLn x = case x of
                   LongNote _ _ _ -> True
                   HitCircle _ _  -> False
        result = Data.List.foldl (\hr ho -> substitute hr (columnH ho) (Just HitNote, if isLn ho then Just (releaseTime ho) else Nothing)) holdsReleases hitObjects
        holdsReleases = Data.List.map (\x -> case x of
                                               Just i  -> if i == tim then (Just ReleaseNote, Nothing) else (Just HoldNote, Just i)
                                               Nothing -> (Nothing, Nothing)) releases
        


--gurdar qnd for hold?
getDistances :: [Chord] -> [(Maybe (Note, Int), Maybe (Note, Int))] -> [[(Maybe (Note, Int), (Maybe (Note, Int)))]]
getDistances [] _ = []
getDistances ((Chord time notes) : next) lasts = result : rest
  where result = Data.List.map (\(x, y) -> (dist x, dist y)) lasts
        rest = (getDistances next　$ Data.List.map (\(x, (ya, yb)) -> case x of
                                                             Nothing       -> (ya, yb)
                                                             Just HoldNote -> (ya, yb)
                                                             Just n        -> (Just (n, time), ya)) (Data.List.zip notes lasts))
  　　    dist y | y == Nothing = Nothing
               | otherwise    = let (Just (ys, t)) = y in Just (ys, time - t)