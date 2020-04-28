import Data.SBV

-- (x,y) numBombs
type Coordinates = (Int, Int)
data Cell = Cell Coordinates Int


{- construct list of constraints corresponding to an individual cell
@todo what is the "type" of a constraint? 
    a predicate or boolean probably... like an equation? or a lambda function?
    yeah... it takes something belonging to the typeclass Provable
ex:
    sat $ \x -> (x :: SInt8) .== (0::SInt8)
-}
-- takes in a cell
-- returns a function from ....
-- how do we represent "variables"???
-- use "forSome"
-- ex:
-- sat . forSome ["x", "y"] $ \ (x::SInteger) y ->
    --  x^2 + y^2 .== 25 &&& 3 * x + 4 * y .== 0
constructEq :: Cell -> 
constructEq = undefined 

-- given a coordinate, returns all adjacent coordinates on the board 
-- For now, assume 5x5 board 
getNeighborCells :: Coordinates -> [Coordinates]
getNeighborCells c = [(x,y) | x <- [0..4], y <- [0..4], adjacent c (x,y) ]
    where
        adjacent :: Coordinates -> Coordinates -> Bool
        adjacent a@(ax, ay) b@(bx, by) = (abs(ax - bx) <= 1) && (abs(ay - by) <= 1) && (a /= b)
                                        