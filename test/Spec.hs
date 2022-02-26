import Test.Hspec

import Parser
import TypeChecker

main :: IO ()
main = hspec $ do
  describe "Parsing expressions" $ do
    describe "all accepted litterals" $ do
      it "parses int litterals" $ do
        parseExpression "2" `shouldBe` Right (Litteral (Num "2"))
        parseExpression "5" `shouldBe` Right (Litteral (Num "5"))
        parseExpression "12" `shouldBe` Right (Litteral (Num "12"))

      it "parses string litterals" $ do
        parseExpression "\"Hello\"" `shouldBe` Right (Litteral (Str "Hello"))
        parseExpression "\"12\"" `shouldBe` Right (Litteral (Str "12"))
        parseExpression "\"\"" `shouldBe` Right (Litteral (Str ""))

      it "parses char litterals" $ do
        parseExpression "'c'" `shouldBe` Right (Litteral (Char "c"))
        parseExpression "'3'" `shouldBe` Right (Litteral (Char "3"))

    describe "functions" $ do
      describe "definiting a new function" $ do
        it "const lambda" $ do
          parseExpression "\\x -> 2"
            `shouldBe`
            Right
              (Lambda
                (Name "x")
                (Litteral (Num "2"))
              )

        it "id lambda" $ do
          parseExpression "\\x -> x"
            `shouldBe`
            Right
              (Lambda
                (Name "x")
                (NameReference (Name"x"))
              )

        it "lambda with multi-token right hand side" $ do
          parseExpression "\\x -> f x"
            `shouldBe`
            Right
              (Lambda
                (Name "x")
                (Apply
                  (NameReference (Name"f"))
                  (NameReference (Name"x")))
              )

      describe "applying a function to a parameter" $ do
        it "lambda application" $ do
          parseExpression "(\\x -> x) 3"
            `shouldBe`
            Right (
              Apply
                (Lambda
                  (Name "x")
                  (NameReference (Name "x"))
                )
                (Litteral (Num "3"))
              )

        it "multiple token application" $ do
          parseExpression "f g h"
            `shouldBe`
            Right (
              Apply
                (Apply
                  (NameReference (Name "f"))
                  (NameReference (Name "g")))
                (NameReference (Name "h"))
              )

        it "priority between application and lambda's rhs" $ do
          --TODO: Do we really want to allow un-parenthesized lambdas like that?
          --      This can be fixed at a later stage, since this is
          parseExpression "f \\x -> g x"
            `shouldBe`
            Right (
              Apply
                (NameReference (Name "f"))
                (Lambda
                  (Name "x")
                  (Apply
                    (NameReference (Name "g"))
                    (NameReference (Name "x")))
                )
              )

    describe "Naming expression" $ do
      it "using a named expression" $ do
        parseExpression "hello" `shouldBe` Right (NameReference (Name "hello"))

  describe "Parser Debug" $ do
    it "lambda application" $ do
      tutu <- testParser "(\\x -> x) 3"
      tutu `shouldBe` ()

(§) a b = a <> "\n" <> b


