module Conways (loop, createGrid)
where

import Control.Concurrent
import qualified Data.List as L
import qualified Data.Vector as V

data Cell = Dead | Alive deriving (Eq, Show)
-- data Grid = [[Cell]]
type Size = (Int, Int)
type Pos = (Int, Int)
type VectorOfCells = V.Vector Cell
type GridOfCells = V.Vector VectorOfCells

evolveCell :: Int -> Cell -> Cell
evolveCell numNeighbors Alive 
    | numNeighbors == 2 || numNeighbors == 3 = Alive
    | otherwise = Dead
evolveCell numNeighbors Dead
    | numNeighbors == 3 = Alive
    | otherwise = Dead

neighbors :: Size -> Pos -> [Pos]
neighbors (numRows, numCols) (row, col) = [
    ( mod (numRows + row + deltaRow) numRows, 
      mod (numCols + col + deltaCol) numCols
    ) | deltaRow <- [-1, 0, 1], deltaCol <- [-1, 0, 1], (deltaRow, deltaCol) /= (0, 0) ]

-- countNeighbors :: Grid -> Pos -> Int
countNeighbors grid pos = (sum . map (valueOfCell . getCell)) getNeighbors where
    size = (V.length grid, V.length (grid V.! 0))
    getNeighbors = neighbors size pos
    getCell = (\(row, col) -> grid V.! row V.! col)
    valueOfCell Alive = 1
    valueOfCell Dead = 0

-- evolve :: GridOfCells -> GridOfCells
evolve grid = V.imap evolveRow grid where
    countGridNeighbors = countNeighbors grid
    evolveOne row col = evolveCell (countGridNeighbors (row, col))
    evolveRow = (\row rowOfCells -> (V.imap (evolveOne row) rowOfCells))

--
-- Render
--

intToCell :: Int -> Cell
intToCell 1 = Alive
intToCell 0 = Dead

toVectorOfCells :: [Int] -> VectorOfCells
toVectorOfCells = V.fromList . map intToCell

createGrid :: [[Int]] -> GridOfCells
createGrid seed = V.fromList (map toVectorOfCells seed)

showGrid :: GridOfCells -> String
showGrid = lines . gridToStrings where
    lines = L.intercalate "\n"
    gridToStrings = V.toList . (V.map rowToString)
    rowToString = V.toList . (V.map cellToChar)
    cellToChar Alive = 'O'
    cellToChar Dead = '.'

loop :: GridOfCells -> IO ()
loop grid = do 
    putStrLn (showGrid grid)
    putStrLn (concat ["\ESC[", (show (V.length (grid V.! 0))), "D\ESC[", show (1 + V.length grid), "A"])
    threadDelay 100000
    -- threadDelay 100000
    loop (evolve grid)    