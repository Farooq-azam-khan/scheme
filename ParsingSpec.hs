module ParsingSpec where 

import Test.Hspec
import Main hiding (main)

main :: IO () 
main = hspec $ do 
    describe "Find Value for valid input" $ do 
        it "should ignore whitespace to any given input" $
            readExpr "   !" `shouldBe` "Found Value"
        
        -- it "should find '|'" $ 
        --     readExpr "|" `shouldBe` "Found Value"