import Data.SBV
import Data.Char
import Data.List
import Data.IORef
import Data.Array.IO
import Control.Monad
import System.Random

-- (x,y) numBombs
type Coordinates = (Int, Int)
data Cell = Cell { coords :: Coordinates
                 , numBombs :: Int
                 } deriving (Show, Eq)

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

-- Need to make a shuffle list method

-- given a board's dimensions and number of bombs, return a list of random bomb cells
--getBombCells :: Int -> Int -> Int -> [Cell]

-- given a coordinate, returns all adjacent coordinates on the board
getNeighborCoordinates :: Coordinates -> [Coordinates]
getNeighborCoordinates c = [(x,y) | x <- [0.. (boardWidth -1)], y <- [0..(boardLength)], adjacent c (x,y)]
    where
        adjacent :: Coordinates -> Coordinates -> Bool
        adjacent a@(ax, ay) b@(bx, by) = (abs(ax - bx) <= 1) && (abs(ay - by) <= 1) && (a /= b)

-- given a string, returns the coordinate it represents
convertStringToCoordinates :: String -> Coordinates
convertStringToCoordinates input = (coordinates1, coordinates2)
    where
    coordinates = filter isNumber input
    coordinates1 = digitToInt (coordinates!!0)
    coordinates2 = digitToInt (coordinates!!1)

--Given two lists, returns the number of matching elements
countMatchingElements :: (Eq a) => [a] -> [a] -> Int
countMatchingElements lst1 lst2 = length lst2 - length (lst2 \\ lst1)

-- Given a coordinates and the locations of the bombs, returns a cell
convertCoordinatesToCell :: Coordinates -> [Coordinates] -> Cell
convertCoordinatesToCell coords bombCells
    | coords `elem` bombCells = Cell coords (-1)
    | otherwise = Cell coords numBombNeighbors
    where neighborCells = getNeighborCoordinates coords
          numBombNeighbors = countMatchingElements bombCells neighborCells

convertCoordinatesToCell' :: [Coordinates] -> Coordinates -> Cell
convertCoordinatesToCell' bombCells coords = convertCoordinatesToCell coords bombCells

convertCoordinatesToUserNumBombs :: [Coordinates] -> [Coordinates] -> Coordinates -> String
convertCoordinatesToUserNumBombs bombCells guesses coords
    | coords `elem` guesses = "|" ++ (show (numBombs (convertCoordinatesToCell coords bombCells))) ++ "|"
    | otherwise = "|?|"

main = do
    putStrLn $ "The board's dimensions are " ++ show boardLength ++ " by " ++ show boardWidth
    putStrLn "What is your first guess?"
    firstGuessString <- getLine
    let firstGuess = convertStringToCoordinates firstGuessString
    shuffledCoordinates <- shuffle allCoordinates
    let mineBombs = createMineBombs firstGuess shuffledCoordinates
    putStrLn $ show mineBombs -- Used for debugging
    let guesses = [firstGuess]
    laterGuesses guesses mineBombs

-- Given
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

nextBestMove :: [Coordinates] -> [Coordinates] -> Maybe Coordinates
nextBestMove guesses mineBombs
    | found = return
    | otherwise = Nothing
    where
    guessCells = map (convertCoordinatesToCell' mineBombs) guesses
    guessNeighborCoords = map getNeighborCoordinates guesses



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

