module MmLibSpec where

import Data.Proxy
import QuickSpec (con, quickSpec, monoType, withMaxTests)
import MmLib
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary, arbitrary, elements, property)

instance Arbitrary Color where
    arbitrary =
        elements [ Red, Blue, Green, Pink, Yellow, Turkis ]

instance Arbitrary Row where
    arbitrary = do
        c1 <- arbitrary
        c2 <- arbitrary
        c3 <- arbitrary
        c4 <- arbitrary
        pure $ Row (c1, c2, c3, c4)

instance Arbitrary Result where
    arbitrary = do
        nrp <- arbitrary
        nrc <- arbitrary
        pure $ Result { numRightPositions = nrp, numRightColors = nrc }

findLaws :: IO ()
findLaws = quickSpec
  [ con "check" (check :: Row -> Row -> Result)
  , monoType (Proxy :: Proxy Row)
  , monoType (Proxy :: Proxy Result)
  , withMaxTests 1000000
  ]

sumResult Result { numRightPositions = p, numRightColors = c } = p + c

unicolorRow color = Row (color, color, color, color)

spec :: Spec
spec = do
  describe "Mastermind test" $ do

    it "GBRP YYGG -> (0, 1)" $ do
        check (Row (Green, Blue, Red, Pink)) (Row (Yellow, Yellow, Green, Green)) `shouldBe` Result { numRightPositions = 0, numRightColors = 1 }

    it "GBRP TTTP -> (1, 0)" $ do
        check (Row (Green, Blue, Red, Pink)) (Row (Turkis, Turkis, Turkis, Pink)) `shouldBe` Result { numRightPositions = 1, numRightColors = 0 }

    it "GBRP PRRY -> (1, 1)" $ do
        check (Row (Green, Blue, Red, Pink)) (Row (Pink, Red, Red, Yellow)) `shouldBe` Result { numRightPositions = 1, numRightColors = 1 }

    it "GBRP GPGY -> (1, 1)" $ do
        check (Row (Green, Blue, Red, Pink)) (Row (Green, Pink, Green, Yellow)) `shouldBe` Result { numRightPositions = 1, numRightColors = 1 }

    it "GBRP RRPY -> (0, 2)" $ do
        check (Row (Green, Blue, Red, Pink)) (Row (Red, Red, Pink, Yellow)) `shouldBe` Result { numRightPositions = 0, numRightColors = 2 }

    it "RRGG RRYY -> (2, 0)" $ do
        check (Row (Red, Red, Green, Green)) (Row (Red, Red, Yellow, Yellow)) `shouldBe` Result { numRightPositions = 2, numRightColors = 0 }

    it "Can not be more right than four" $ do
        property $ \guess answer -> sumResult (check guess answer) <= 4

    it "Can not be more wrong than zero" $ do
        property $ \guess answer -> sumResult (check guess answer) >= 0

    it "Unicolor rows is either all right or all wrong" $ do
        property $ \guess answer ->
            let checked = check (unicolorRow guess) (unicolorRow answer)
            in  checked == Result { numRightPositions = 4, numRightColors = 0 } ||
                checked == Result { numRightPositions = 0, numRightColors = 0 }

    it "Correct guess gives 4 positions right else less" $ do
        property $ \guess answer ->
            let checked = check guess answer
            in  if guess == answer then
                    numRightPositions checked == 4
                else
                    numRightPositions checked < 4

    -- Found by QuickSpec

    it "check x y = check y x" $ do
        property $ \x y ->
            check x y == check y x

    it "check x x = check y y" $ do
        property $ \x y ->
            check x x == check y y
