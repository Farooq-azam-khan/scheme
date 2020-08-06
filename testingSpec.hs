module TestingSpec where 

--- broken tests due to IO output

import Test.Hspec
import Main hiding (main)
import Parser

main :: IO () 
main = hspec $ do 
    describe "Find Value for valid input" $ do 
        -- currently broken
    --     it "should ignore whitespace to any given input" $
    --         lispExecute "   !" `shouldBe` "Found Value"
        
        it "should find '!'" $ 
            lispExecute "!" `shouldBe` "Atom \"!\""
        
        it "should find (String \"abc\")" $ 
            lispExecute "abc" `shouldBe` "Atom \"abc\""

        it "should find (String \"abc def\")" $ 
            lispExecute "abc def" `shouldBe` "Atom \"abc\""
        
        it "should find True #t" $ 
            lispExecute "#t" `shouldBe` "Bool True"
        
        it "should find False #f" $ 
            lispExecute "#f" `shouldBe` "Bool False"

        it "should read tab (\\t)" $
            lispExecute "\"\t\"" `shouldBe` "String \"\\t\""
        
        it "should read carriage return (\\r)" $
            lispExecute "\"\r\"" `shouldBe` "String \"\\r\""

        it "should read new line (\\n)" $
            lispExecute "\"\n\"" `shouldBe` "String \"\\n\""
        
        it "should read backslash (\\)" $
            lispExecute "\" \\\\ \"" `shouldBe` "String \" \\\\ \""

        it "should read decimal number 123" $
            lispExecute "123" `shouldBe` "Number 123"

        it "should read explicit decimal number 123" $
            lispExecute "#d123" `shouldBe` "Number 123"
        
        it "should read hex ABC" $
            lispExecute "#xABC" `shouldBe` "Number 291"
        
        it "should read oct 123" $
            lispExecute "#o123" `shouldBe` "Number 83"
        
        it "should read bin 11" $
            lispExecute "#b11" `shouldBe` "Number 3"

        it "should read a float" $
            lispExecute "123.123" `shouldBe` "Float 123.123"
        
        it "should parse List (123 123 123)" $ 
            lispExecute "(123 123 123)" `shouldBe` "List [Number 123,Number 123,Number 123]"

        it "should parse Nested List" $ 
            lispExecute "(\"a\" (nested) test)" `shouldBe` "List [String \"a\",List [Atom \"nested\"],Atom \"test\"]"
            
        it "should parse DottedList (123 123 123)" $ 
            lispExecute "(123 123 . 123)" `shouldBe` "DottedList [Number 123,Number 123] (Number 123)"
        
        -- todo: eval is messing with it and ' is being parsed properly 
        it "should parse Quotted" $ 
            lispExecute "'abcdef" `shouldBe` "List [Atom \"quote\",Atom \"abcdef\"]"

        it "should parse list and dotted list" $ 
            lispExecute "(\"a\" (dotted . list) test)" `shouldBe` "List [String \"a\",DottedList [Atom \"dotted\"] (Atom \"list\"),Atom \"test\"]"
        
    describe "Evaluate proper input and output evaluation" $ do 

        it "should evaluate (+ 2 2)" $
            lispExecute "(+ 2 2)" `shouldBe` "Number 4"

        it "should evaluate (- 2 2)" $
            lispExecute "(- 2 2)" `shouldBe` "Number 0"
            
        it "should evaluate (* 2 2)" $
            lispExecute "(* 2 2)" `shouldBe` "Number 4"
        
        it "should evaluate more than 2 numbers (+ 1 2 3)" $
            lispExecute "(+ 1 2 3)" `shouldBe` "Number 6"

        it "should check equlity (eq? 1 1)" $
            lispExecute "(= 1 1)" `shouldBe` "Bool True"
        
        it "should evaluate floats" $
            lispExecute "(addFloat 2.1 2.1)" `shouldBe` "Float 4.2"
    
    describe "Spec can  evaluate errors properly" $ do 
        it "should evaluate (+ 2 \"two\")" $ 
            lispExecute "(+ 2 \"two\")" `shouldBe` "TypeMismatch \"number\" (String \"two\")"
    
    describe "Spec can pattern match eval conditions" $ do 
        it "should check if true conditions" $
            lispExecute "(if (< 1 2) #t #f)" `shouldBe` "Bool True"

        it "should check if false conditions" $
            lispExecute "(if (> 1 2) #t #f)" `shouldBe` "Bool False"

        it "should check if condition and return string" $ 
            lispExecute "(if (= 1 2) \"equal\" \"not equal\")" `shouldBe` "String \"not equal\""

    describe "Spec should work with variable" $ do 
        it "should define varaible and add with it" $ do
            x <- lispExecute "(define x 3)"
            lispExecute "(+ x 3)" `shouldBe` "Number 6" 

