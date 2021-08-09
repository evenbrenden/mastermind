module MmLibSpec where

import MmLib
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Color where
    arbitrary =
        elements [ Red, Blue, Green, Pink, Yellow, Turkis ]

sumResult Result { numRightPositions = p, numRightColors = c } = p + c

unicolorRow color = (color, color, color, color)

spec :: Spec
spec = do
  describe "Mastermind test" $ do

    it "GBRP YYGG -> (0, 1)" $ do
        check (Green, Blue, Red, Pink) (Yellow, Yellow, Green, Green) `shouldBe` Result { numRightPositions = 0, numRightColors = 1 }

    it "GBRP TTTP -> (1, 0)" $ do
        check (Green, Blue, Red, Pink) (Turkis, Turkis, Turkis, Pink) `shouldBe` Result { numRightPositions = 1, numRightColors = 0 }

    it "GBRP PRRY -> (1, 1)" $ do
        check (Green, Blue, Red, Pink) (Pink, Red, Red, Yellow) `shouldBe` Result { numRightPositions = 1, numRightColors = 1 }

    it "GBRP GPGY -> (1, 1)" $ do
        check (Green, Blue, Red, Pink) (Green, Pink, Green, Yellow) `shouldBe` Result { numRightPositions = 1, numRightColors = 1 }

    it "GBRP RRPY -> (0, 2)" $ do
        check (Green, Blue, Red, Pink) (Red, Red, Pink, Yellow) `shouldBe` Result { numRightPositions = 0, numRightColors = 2 }

    it "RRGG RRYY -> (2, 0)" $ do
        check (Red, Red, Green, Green) (Red, Red, Yellow, Yellow) `shouldBe` Result { numRightPositions = 2, numRightColors = 0 }

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
                    checked == Result { numRightPositions = 4, numRightColors = 0 }
                else
                    numRightPositions checked < 4

    it "Argument order does not matter" $ do
        property $ \guess answer ->
            let checkedXY = check guess answer
                checkedYX = check answer guess
            in checkedXY == checkedYX
