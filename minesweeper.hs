import Data.SBV

-- (x,y) numBombs
type Coordinates = (Int, Int)
data Cell = Cell Coordinates Int

{- For now, assume 5x5 board -}

-- construct list of constraints corresponding to an individual cell
constructEq :: Cell -> undefined
constructEq = undefined 


getNeighborCells :: Coordinates -> [Coordinates]
getNeighborCells c = [(x,y) | x <- [0..4], y <- [0..4], adjacent c (x,y) ]
    where
        adjacent :: Coordinates -> Coordinates -> Bool
        adjacent a@(ax, ay) b@(bx, by) = (abs(ax - bx) <= 1) && (abs(ay - by) <= 1) && (a /= b)
                                        