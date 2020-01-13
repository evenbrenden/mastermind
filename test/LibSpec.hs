module LibSpec where

import Test.Hspec
import Test.QuickCheck
import Lib

instance Arbitrary Color where
    arbitrary =
        elements [ Red, Blue, Green, Pink, Yellow, Turkis ]

sumResult result = fst result + snd result

unicolorRow color = (color, color, color, color)

spec :: Spec
spec = do
  describe "Mastermind test" $ do

    it "GBRP YYGG -> (0, 1)" $ do
        check (Green, Blue, Red, Pink) (Yellow, Yellow, Green, Green) `shouldBe` (0, 1)

    it "GBRP PRRY -> (1, 1)" $ do
        check (Green, Blue, Red, Pink) (Pink, Red, Red, Yellow) `shouldBe` (1, 1)

    it "GBRP GPGY -> (1, 1)" $ do
        check (Green, Blue, Red, Pink) (Green, Pink, Green, Yellow) `shouldBe` (1, 1)

    it "GBRP RRPY -> (1, 1)" $ do
        check (Green, Blue, Red, Pink) (Red, Red, Pink, Yellow) `shouldBe` (1, 1)

    it "Can not be more right than four" $ do
        property $ \g a -> sumResult (check g a) <= 4

    it "Can not be more wrong than zero" $ do
        property $ \g a -> sumResult (check g a) >= 0

    it "Unicolor rows is either all right or all wrong" $ do
        property $ \g a ->
            let checked = check (unicolorRow g) (unicolorRow a)
            in checked == (0, 0) || checked == (4, 0)

    it "Correct guess => 4 positions right else less" $ do
        property $ \g a ->
            let checked = check g a
            in if g == a then checked == (4, 0) else fst checked < 4
