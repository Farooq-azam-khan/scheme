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
            readExpr "!" `shouldBe` Atom "!"
        
        it "should find (String \"abc\")" $ 
            readExpr "abc" `shouldBe` Atom "abc"

        it "should find (String \"abc def\")" $ 
            readExpr "abc def" `shouldBe` Atom "abc"
        
        it "should find True #t" $ 
            readExpr "#t" `shouldBe` Bool True
        
        it "should find False #f" $ 
            readExpr "#f" `shouldBe` Bool False

        it "should read tab (\\t)" $
            readExpr "\"\t\"" `shouldBe` String "\t"
        
        it "should read carriage return (\\r)" $
            readExpr "\"\r\"" `shouldBe` String "\r"

        it "should read new line (\\n)" $
            readExpr "\"\n\"" `shouldBe` String "\n"
        
        it "should read backslash (\\)" $
            readExpr "\" \\\\ \"" `shouldBe` String " \\ "

        it "should read decimal number 123" $
            readExpr "123" `shouldBe` Number 123
        
        it "should read explicit decimal number 123" $
            readExpr "#d123" `shouldBe` Number 123
        
        it "should read hex ABC" $
            readExpr "#xABC" `shouldBe` Number 291
        
        it "should read oct 123" $
            readExpr "#o123" `shouldBe` Number 83
        
        it "should read bin 11" $
            readExpr "#b11" `shouldBe` Number 3

        it "should read a float" $
            readExpr "123.123" `shouldBe` Float 123.123
        
        it "should parse List (123 123 123)" $ 
            readExpr "(123 123 123)" `shouldBe` List [Number 123,Number 123,Number 123]

        it "should parse Nested List" $ 
            readExpr "(a (nested) test)" `shouldBe` List [Atom "a",List [Atom "nested"],Atom "test"]
            
        it "should parse DottedList (123 123 123)" $ 
            readExpr "(123 123 . 123)" `shouldBe` DottedList [Number 123,Number 123] (Number 123)
        
        it "should parse Quotted" $ 
            readExpr "'abcdef" `shouldBe` List [Atom "quote",Atom "abcdef"]

        it "should parse list and dotted list" $ 
            readExpr "(a (dotted . list) test)" `shouldBe` List [Atom "a",DottedList [Atom "dotted"] (Atom "list"),Atom "test"]
        
        it "should evaluate (+ 2 2)" $
            (eval . readExpr) "(+ 2 2)" `shouldBe` Number 4
