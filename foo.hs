import Data.SBV

-- 2 * x == x + x
import Data.SBV
x = prove . forAll ["x"] $ \(x::SWord8) -> 2 * x .== x + x + x

someEquation :: SWord8 -> SWord8 -> SWord8 -> SBool
someEquation a b c = sAnd [((a + b) .== c), (a .> 0)] 

-- dubble :: Integer -> SBool
-- dubble x = ((x + x) :: SWord8) .== ((2 * x) :: SWord8)

-- do [x, y, z] <- sIntegers ["x", "y", "z"]
-- solve [x .> 5, y + z .< x]

-- eq = solve [x .> y, z .< x]
--     where 
--         x = sInteger "x"
--         y = sInteger "y"
--         z = sInteger "z"


-- can repeat variable names
-- sat . forSome ["x", "y"] $ \ (x::SInteger) y -> x^2 + y^2 .== 25 .&& 3 * x + 4 * y .== 0

-- https://ocharles.org.uk/guest-posts/2013-12-09-24-days-of-hackage-sbv.html

example :: IO SatResult
example = sat $ do x <- sIntegers ["x", "x"]
                   constrain $ x!!0 .== (literal 10)
                   constrain $ x!!1 ./= (10 :: SInteger)

                   -- Now add soft-constraints to indicate our preference
                   -- for what these variables should be:
                   softConstrain $ x!!0 .== 0
                   softConstrain $ x!!1 .== -1

                   return sTrue


sumConstraint :: [SInteger] -> SBool
sumConstraint l = sum l .> 10

maxConstraint :: [SInteger] -> SBool
maxConstraint l = sum l .< 20

manyConstraints :: IO SatResult
manyConstraints = sat $ do 
                    x <- sIntegers ["x", "x"]
                    mapM_ constrain [(sumConstraint x), (maxConstraint x)]
                    return sTrue
