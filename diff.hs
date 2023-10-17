import System.Environment
import Data.Text hiding (head, tail)
import Data.Char
import Data.List
import System.IO
import Data.Ratio

main = do
  (mapPath : diffPath : _) <- getArgs
  mapFile <- openFile mapPath ReadMode
  diffFile <- openFile diffPath ReadMode
  hSetEncoding mapFile utf8
  hSetEncoding diffFile utf8
  mapContents <- hGetContents mapFile
  diffContents <- hGetContents diffFile

  let timingsLine = snd $ Prelude.break ("[TimingPoints]"==) $ Data.List.lines mapContents
  let timingPoints = getTimingPoints timingsLine
  let notesLine = tail $ snd $ Prelude.break ("[HitObjects]"==) timingsLine
  let chords = readHitObjects notesLine 10
  let chords2 = splitChord 10 chords
  let diffLines = Data.List.lines diffContents
  let hitHitDiffArgs = getDiffFunctionArgs 0 $ tail $ snd $ Prelude.break ("[hithit]"==) diffLines
  let hitRelDiffArgs = getDiffFunctionArgs 0 $ tail $ snd $ Prelude.break ("[hitrel]"==) diffLines
  let relHitDiffArgs = getDiffFunctionArgs 0 $ tail $ snd $ Prelude.break ("[relhit]"==) diffLines
  let relRelDiffArgs = getDiffFunctionArgs 0 $ tail $ snd $ Prelude.break ("[relrel]"==) diffLines
  let holdDiffArgs = getDiffFunctionArgs 0 $ tail $ snd $ Prelude.break ("[hold]"==) diffLines
  let distsL = getDistances (fst chords2) 0 (Data.List.replicate 5 (-1, -1))
  let distsR = getDistances (snd chords2) 0 (Data.List.replicate 5 (-1, -1))
  
  putStrLn $ show $ getDiff (hitHitDiffArgs, hitRelDiffArgs, relHitDiffArgs, relRelDiffArgs, holdDiffArgs) (fst chords2) distsL 0
  putStrLn $ show $ getDiff (hitHitDiffArgs, hitRelDiffArgs, relHitDiffArgs, relRelDiffArgs, holdDiffArgs) (snd chords2) distsR 0
  
  
  --putStrLn $ show $ getTimingPoints timingsLine
  --putStrLn $ show $ readHitObjects notesLine 10

splitChord :: Int -> [Chord] -> ([Chord], [Chord])
splitChord key chords = Data.List.unzip $ Data.List.map (\x -> let (t, (y1, y2)) = (time x, (Prelude.splitAt half $ notes x)) in (Chord t (reverse (if mod key 2 == 0 then y1 else Nothing : y1)), Chord t y2)) chords
  where half = div key 2
        reverse a = Prelude.foldl (\x y -> y : x) [] a

getDiffFunctionArgs :: Int -> [String] -> [[(Double, Double, Double)]]
getDiffFunctionArgs _ [] = []
getDiffFunctionArgs i (s:ss) | s == ""   = []
                             | otherwise = curr : (getDiffFunctionArgs (i + 1) ss)
  where curr = Data.List.map (\x -> let temp = Data.List.map read $ Data.List.words x :: [Double] in (temp !! 0, temp !! 1, temp !! 2)) $ splitOnDelimiter s "|"

riceFunc :: Double -> Double -> Double -> Int -> Int
riceFunc a b c x = round (-x1 * x1 * a + x1 * b + c)
  where x1 = fromIntegral x

jackFunc :: Double -> Double -> Double -> Int -> Int
jackFunc a b c x = round (-a * (logBase b (c * x1)))
  where x1 = fromIntegral x
  
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
        


getDistances :: [Chord] -> Int -> [(Int, Int)] -> [(Int, [(Int, Int)])]
getDistances [] _ _ = []
getDistances ((Chord time notes) : next) i lasts = (i, lasts) : rest
  where rest = (getDistances next (i + 1)　$ Data.List.map (\(x, (ya, yb)) -> case x of
                                                                               Nothing       -> (ya, yb)
                                                                               Just HoldNote -> (ya, yb)
                                                                               Just n        -> (i, ya)) (Data.List.zip notes lasts))


getDiff :: ([[(Double, Double, Double)]], [[(Double, Double, Double)]], [[(Double, Double, Double)]], [[(Double, Double, Double)]], [[(Double, Double, Double)]]) -> [Chord] -> [(Int, [(Int, Int)])] -> Int -> [([[Int]], [[Double]])]
getDiff _ _ [] _ = []
getDiff (hitHitArgs, hitRelArgs, relHitArgs, relRelArgs, holdArgs) chords ((curr, di):sts) threshold = ((getNotesDiff $ fst hitReleasesHolds), (getHoldDiff hitReleasesHolds)) : (getDiff (hitHitArgs, hitRelArgs, relHitArgs, relRelArgs, holdArgs) chords sts threshold)
  where currChord = chords !! curr
        hitReleasesHolds = fst $ Data.List.foldl (\((x, xx), c) y -> case y of
          Nothing          -> ((x, xx), c + 1)
          Just HitNote     -> ((x ++ [c], xx), c + 1)
          Just ReleaseNote -> ((x ++ [c], xx), c + 1)
          otherwise        -> ((x, xx ++ [c]), c + 1)
            ) (([], []), 0) $ notes currChord

        getHoldDiff (x, z) = Data.List.map (\zs -> Data.List.map (\xs -> applyHold zs xs) x) z
        applyHold c1 c = let (a, _, _) = holdArgs !! c1 !! c in a

        getNotesDiff arr = Data.List.map (diffs) arr
        diffs c1 = let r = Data.List.foldl (applyFunc c1) ([], 0) di in Data.List.map (max 0) (fst r) 
        applyFunc c1 (x, c) (y, ys) = let t = tryApply c1 c y; ts = tryApply c1 c ys in (x ++ [if t < 0 then (Prelude.max ts 0) else t], c + 1)

        tryApply c1 c y | y < 0                = -1
                        | c1 == c              = let (a1, a2, a3) = (argsDecision c1 c y) !! c1 !! c in jackFunc a1 a2 a3 $ distms y
                        | distms y < threshold = -1
                        | otherwise            = let (a1, a2, a3) = (argsDecision c1 c y) !! c1 !! c in riceFunc a1 a2 a3 $ distms y

        
        argsDecision c1 c y = let t1 = (notes currChord) !! c; t = (notes $ chords !! y) !! c
                                in case t1 of
                                        Just HitNote -> if t == Just HitNote then hitHitArgs else hitRelArgs
                                        otherwise    -> if t == Just HitNote then relHitArgs else relRelArgs

        distms ind = (time $ chords !! curr) - (time $ chords !! ind)