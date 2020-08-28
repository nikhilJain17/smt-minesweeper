--import Data.SBV
import Data.Char
import Data.List
import Data.IORef
import Data.Array.IO
import Control.Monad
import System.Random

-- (x,y) numBombs
type Coordinates = (Int, Int)
data Cell = Cell Coordinates Int deriving (Show, Eq)

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
--constructEq :: Cell ->
--constructEq = undefined

boardLength :: Int
boardLength = 7

boardWidth :: Int
boardWidth = 7

numBombs = 4

rows = [0 .. (boardLength - 1)]
cols = [0 .. (boardWidth - 1)]

allCoordinates = [(x,y) | x <- rows, y <- cols]
actualBoard = [ [convertCoordinatesToCell (x,y) | y <- cols] | x <- rows]

-- given a coordinate, returns all adjacent coordinates on the board
getNeighborCoordinates :: Coordinates -> [Coordinates]
getNeighborCoordinates c = [(x,y) | x <- [0..4], y <- [0..4], adjacent c (x,y)]
    where
        adjacent :: Coordinates -> Coordinates -> Bool
        adjacent a@(ax, ay) b@(bx, by) = (abs(ax - bx) <= 1) && (abs(ay - by) <= 1) && (a /= b)

convertStringToCoordinates :: String -> Coordinates
convertStringToCoordinates input = (coordinates1, coordinates2)
    where
        coordinates = filter isNumber input
        coordinates1 = digitToInt (coordinates!!0)
        coordinates2 = digitToInt (coordinates!!1)

countMatchingElements :: (Eq a) => [a] -> [a] -> Int
countMatchingElements lst1 lst2 = length lst2 - length (lst2 \\ lst1)

-- helper function to set up the board by setting up a single cell
convertCoordinatesToCell :: Coordinates -> [Coordinates] -> Cell
convertCoordinatesToCell coords bombCells
    | coords `elem` bombCells = Cell coords (-1)
    | otherwise = Cell coords numBombNeighbors
    where neighborCells = getNeighborCoordinates coords
          numBombNeighbors = countMatchingElements bombCells neighborCells

-- process a users input
convertCoordinatesToUserNumBombs :: [Coordinates] -> [Coordinates] -> Coordinates -> String
convertCoordinatesToUserNumBombs bombCells guesses coords
    | coords `elem` guesses = "|" ++ (show (convertCellToNumBombs (convertCoordinatesToCell coords bombCells))) ++ "|"
    | otherwise = "|?|"

convertCellToNumBombs :: Cell -> Int
convertCellToNumBombs (Cell _ numBombs) = numBombs

main = do
    putStrLn $ "The board's dimensions are " ++ show boardLength ++ " by " ++ show boardWidth
    putStrLn "What is your first guess?"
    firstGuessString <- getLine
    let firstGuess = convertStringToCoordinates firstGuessString
    shuffledCoordinates <- shuffle allCoordinates
    let mineBombs = createMineBombs firstGuess shuffledCoordinates
    putStrLn $ show mineBombs
    let guesses = [firstGuess]
    laterGuesses guesses mineBombs

laterGuesses :: [Coordinates] -> [Coordinates] -> IO Bool
laterGuesses guesses mineBombs =  do
    putStrLn $ boardString guesses mineBombs
    putStrLn "What is your next guess?"
    guessString <- getLine
    let guess = convertStringToCoordinates guessString
    let boolHit = checkHit guess mineBombs
    let newGuesses = guess:guesses
    if boolHit
        then return False
        else if length newGuesses == boardLength * boardWidth - length mineBombs
        then return True
        else laterGuesses newGuesses mineBombs


checkHit:: Coordinates -> [Coordinates] -> Bool
checkHit guess mineBombs
    | guess `elem` mineBombs = True
    | otherwise = False

boardString :: [Coordinates] -> [Coordinates] -> String
boardString guesses mineBombs = finalString
    where
    allCoordinates = [ [(x,y) | y <- cols] | x <- rows]
    allUserNumBombs = map (convertCoordinatesList guesses mineBombs) allCoordinates
    allUserNumBombsStrings = map (intercalate " ") allUserNumBombs
    finalString = intercalate "\n" allUserNumBombsStrings

convertCoordinatesList :: [Coordinates] -> [Coordinates] -> [Coordinates] -> [String]
convertCoordinatesList guesses mineBombs list = map (convertCoordinatesToUserNumBombs mineBombs guesses) list

-- Taken from web
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs

createMineBombs :: Coordinates -> [Coordinates] -> [Coordinates]
createMineBombs firstGuess shuffled =
    let possibleCoordinates = filter (/= firstGuess) shuffled
    in take numBombs possibleCoordinates

-------------------------------------------
-- smt related stuff
-------------------------------------------

checkIfBombCandidate :: Coordinates -> SBool
checkIfBombCandidate = undefined
-- first, make all constraints possible from current state of board
    -- a constraint is possible if we know the number of neighbors (i.e. )
-- then, place bomb and sat it



-- make a single equation from a cell 
makeEquationFromCell :: Cells -> SBool
makeEquation (Cell coords count) = undefined
    where
        numOfSurroundingBombs = undefined
        freeVariables :: [] -- use sIntegers :: [String] -> Symbolic [SInteger]Source
-- if cell has number:
    -- sum of bombs around cell == cell number
    -- cell with no bomb has value of zero
    -- unopened cell is a free variable

    
-- do [x, y, z] <- sIntegers ["x", "y", "z"]
-- solve [x .> 5, y + z .< x]