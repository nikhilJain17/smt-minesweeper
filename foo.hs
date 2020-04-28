import Data.SBV

-- 2 * x == x + x
prove . forAll ["x"] $ \(x::SWord8) -> 2 * x .== x + x
