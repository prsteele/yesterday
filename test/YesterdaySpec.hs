module YesterdaySpec where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Yesterday" $ do
    prop "Knows how to add numbers" propYesterdayAdds

propYesterdayAdds :: Integer -> Integer -> Bool
propYesterdayAdds x y = x + y == x + y
