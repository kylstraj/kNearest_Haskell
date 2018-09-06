import Data.List
import System.IO

type Variables = [Double]
data Record = Record { variables :: Variables,
                       outcome :: Double
                     } deriving (Show)
data DataSet = DataSet { records :: [Record] } deriving (Show)
data DistanceOutcome = DistanceOutcome { doDistance :: Double,
                                         doOutcome :: Double } deriving (Show)

distance :: Variables -> Variables -> Double
distance v1 v2 = sqrt (sum (map (\x -> x ** 2) 
                                (zipWith (-) v1 v2)))

distances :: Variables -> DataSet -> [Double]
distances vs dataSet = map (\rec -> distance vs (variables rec))
                           (records dataSet) 

distanceOutcomeFromDuple :: (Double, Double) -> DistanceOutcome
distanceOutcomeFromDuple (distance, outcome) = DistanceOutcome distance outcome

distancesWithOutcomes :: Variables -> DataSet -> [DistanceOutcome]
distancesWithOutcomes vs dataSet = map distanceOutcomeFromDuple duples
  where duples = zip (distances vs dataSet)
                     (map (\rec -> outcome rec)
                          (records dataSet))

compareDistanceOutcomes :: DistanceOutcome -> DistanceOutcome -> Ordering
compareDistanceOutcomes d1 d2
  | (doDistance d1) < (doDistance d2) = LT
  | (doDistance d1) > (doDistance d2) = GT
  | otherwise = EQ

sortDistanceOutcomes :: [DistanceOutcome] -> [DistanceOutcome]
sortDistanceOutcomes distanceOutcomes =
  sortBy compareDistanceOutcomes distanceOutcomes

kNearest :: Int -> Variables -> DataSet -> [DistanceOutcome]
kNearest k vs ds = take k 
                        (sortDistanceOutcomes (distancesWithOutcomes vs ds))

avgKNearest :: Int -> Variables -> DataSet -> Double
avgKNearest k vs ds = avg (map doOutcome (kNearest k vs ds))
  where
    avg xs = ((sum xs) / (fromIntegral (length xs)) :: Double)

strToRcd :: Char -> String -> Record
strToRcd delim str = Record (init nums) (last nums)
  where
    nums = map (\ tk -> read tk :: Double) (split delim str)

strToRcdSpaces = strToRcd ' '
strToRcdCommas = strToRcd ','

consumeData :: Handle -> Char -> [Record] -> IO DataSet
consumeData hdl delim rcds =
  do eof <- hIsEOF hdl
     if eof
        then return (DataSet rcds)
        else do rcdStr <- hGetLine hdl
                consumeData hdl delim (((strToRcd delim) rcdStr):rcds)

split :: Char -> String -> [String]
split delim str = 
  let (token, rest) = break isDelimeter str
  in token : case rest of
               [] -> []
               (c:cs) -> split delim cs

  where
    isDelimeter c = c == delim
  
kNearestFromFile :: String -> Variables -> Int -> Char -> IO ()
kNearestFromFile filename vs k delim =
  do hdl <- openFile filename ReadMode
     ds <- consumeData hdl delim []
     let ans = avgKNearest k vs ds
     putStrLn (show ans)
