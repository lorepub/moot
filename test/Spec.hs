import           TestImport

main :: IO ()
main = hspec $ do
    describe "2"  $ do
        it "equals 2" $ do
            2 `shouldBe` 2
