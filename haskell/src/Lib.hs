module Lib
    ( someFunc
    , updateField
    , printField
    , blinker
    , Field
    ) where
import Data.Maybe (isJust, fromJust)
import System.Console.ANSI (setCursorPosition, clearScreen)
import Data.Data ((:~~:)(HRefl))

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Cell = Alive | Dead deriving (Show, Eq)
type Field = [Cell]


blinker = [ Dead, Dead, Dead, Dead, Dead
          , Dead, Dead, Dead, Dead, Dead
          , Dead, Alive, Alive, Alive, Dead
          , Dead, Dead, Dead, Dead, Dead
          , Dead, Dead, Dead, Dead, Dead
          ]

fieldWidth :: Int
fieldWidth = 5

updateField :: Field -> Field
updateField field = map (updateCell field) [0..(fieldWidth*fieldWidth)-1]

printField :: Field -> IO ()
printField field = do
  clearScreen
  mapM_ (\x -> printCell (snd x) (fst x)) $ enumerate field
  putStrLn "\n"

printCell :: Cell -> Int -> IO ()
printCell cell id_ | cell == Alive = do
                       setCursorPosition y x
                       putStr "0"
                   | otherwise = return ()
  where y = id_ `div` fieldWidth
        x = id_ - (y*fieldWidth)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]


updateCell :: Field -> Int -> Cell
updateCell field id_ | isAliveNextGen field id_ = Alive
                     | otherwise = Dead

isAliveNextGen :: Field -> Int -> Bool
isAliveNextGen field id_ | target == Dead  && nAlive == 3 = True
                         | target == Alive && nAlive < 2 = False
                         | target == Alive && nAlive > 3 = False
                         | target == Alive = True
                         | target == Dead = False
  where target = field !! id_
        cells  = getNeighbors field id_
        nAlive = length $ filter (== Alive) cells


getNeighbors :: Field -> Int -> [Cell]
getNeighbors field id_ = getNeighborCells field $ getNeighborIndexes id_

getNeighborCells :: Field -> [Int] -> [Cell]
getNeighborCells _ []     = []
getNeighborCells field (x:xs) = (field !! x) : getNeighborCells field xs


getNeighborIndexes :: Int -> [Int]
getNeighborIndexes i = [fromJust x | x <- [top, right, bottom, left, tl, tr, bl, br], isJust x]
  where top    = getTop i
        right  = getRight i
        bottom = getBottom i
        left   = getLeft i
        tl     = getTopLeft i
        tr     = getTopRight i
        bl     = getBottomLeft i
        br     = getBottomRight i


getTopLeft :: Int -> Maybe Int
getTopLeft i | tl >= 0 && isOnSameRow tl (i-fieldWidth) fieldWidth = Just tl
             | otherwise = Nothing
  where tl = i - fieldWidth - 1

getTopRight :: Int -> Maybe Int
getTopRight i | tr >= 0 && isOnSameRow tr (i-fieldWidth) fieldWidth = Just tr
              | otherwise = Nothing
  where tr = i - fieldWidth + 1

getTop :: Int -> Maybe Int
getTop i | t >= 0 = Just t
         | otherwise = Nothing
  where t = i - fieldWidth


getRight :: Int -> Maybe Int
getRight i | isOnSameRow r i fieldWidth = Just r
           | otherwise = Nothing
  where r = i + 1

getBottom :: Int -> Maybe Int
getBottom i | b < fieldWidth * fieldWidth = Just b
            | otherwise = Nothing
  where b = i + fieldWidth

getBottomLeft :: Int -> Maybe Int
getBottomLeft i | bl < fieldWidth * fieldWidth && isOnSameRow bl (i+fieldWidth) fieldWidth = Just bl
                | otherwise = Nothing
  where bl = i + fieldWidth - 1

getBottomRight :: Int -> Maybe Int
getBottomRight i | br < fieldWidth * fieldWidth && isOnSameRow br (i+fieldWidth) fieldWidth = Just br
                 | otherwise = Nothing
  where br = i + fieldWidth + 1

getLeft :: Int -> Maybe Int
getLeft i | isOnSameRow l i fieldWidth = Just l
          | otherwise = Nothing
  where l = i - 1

isOnSameRow :: Int -> Int -> Int -> Bool
isOnSameRow x y width = x `div` width == y `div` width

