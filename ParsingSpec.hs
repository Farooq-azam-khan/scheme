module ParsingSpec where 

import Test.Hspec
import Main hiding (main)

main :: IO () 
main = hspec $ do 
    describe "Find Value for valid input" $ do 
        -- currently broken
    --     it "should ignore whitespace to any given input" $
    --         readExpr "   !" `shouldBe` "Found Value"
        
        it "should find '!'" $ 
            readExpr "!" `shouldBe` "Found Value"
        
        it "should find (Number 123)" $ 
            readExpr "123" `shouldBe` "Found Value"
        
        it "should find (String \"abc\")" $ 
            readExpr "abc" `shouldBe` "Found Value"

        it "should find (String \"abc def\")" $ 
            readExpr "abc def" `shouldBe` "Found Value"
        
        it "should find True #t" $ 
            readExpr "#t" `shouldBe` "Found Value"
        
        it "should find False #f" $ 
            readExpr "#f" `shouldBe` "Found Value"

        it "should read tab (\\t)" $
            readExpr "\"\t\"" `shouldBe` "Found Value"
        
        it "should read carriage return (\\r)" $
            readExpr "\"\r\"" `shouldBe` "Found Value"

        it "should read new line (\\n)" $
            readExpr "\"\n\"" `shouldBe` "Found Value"
        
        it "should read backslash (\\)" $
            readExpr "\" \\\\ \"" `shouldBe` "Found Value"

        it "should read decimal number 123" $
            readExpr "123" `shouldBe` "Found Value"
        
        it "should read explicit decimal number 123" $
            readExpr "#d123" `shouldBe` "Found Value"
        
        it "should read hex ABC" $
            readExpr "#xABC" `shouldBe` "Found Value"
        
        it "should read oct 123" $
            readExpr "#o123" `shouldBe` "Found Value"
        
        it "should read bin 11" $
            readExpr "#b11" `shouldBe` "Found Value"