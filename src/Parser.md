```haskell
module Parser
  ( parseExpression
  , LitType(..)
  , Expression(..)
  , Name(..)
  , testParser
  ) where

import Data.Functor (void)
import Data.Text (Text, cons)
import Data.Either (Either(..))
import Data.Either.Combinators (mapLeft)
import Data.Function ((&))
import Control.Applicative ((<|>))

import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MPC

type Parser a = MP.Parsec Expression Text a
type Error = Text

newtype Name = Name Text
  deriving (Show, Eq, Ord)
```

Allowed litterals in our program, and their parsers.
So far, no primitive operations are defined, so these types aren't very usable.

```haskell
data LitType
  = Num Text
  | Str Text
  | Char Text
  deriving (Show, Eq, Ord)

litteralParser :: Parser Token
litteralParser =
      MP.try numParser
  <|> MP.try strParser
  <|> charParser

numParser :: Parser Token
numParser = do
    digits <- MP.takeWhile1P Nothing (\c -> c `elem` ("0123456789" :: String) )
    pure $ LitToken (Num digits)

strParser :: Parser Token
strParser = do
  MPC.char '"'
  str <- MP.takeWhileP Nothing (\c -> c /= '"')
  MPC.char '"'
  pure $ LitToken (Str str)

charParser :: Parser Token
charParser = do
  MPC.char '\''
  str <- MP.takeP Nothing 1
  MPC.char '\''
  pure $ LitToken (Char str)
```

because we don't have a special delimiter for the args passed to a function, or
a hardcoded list of infix operators, it is easier to parse a list of intermediate
`Token`s. These tokens can then be converted to expressions.

Tokens are as follows:

```haskell
data Token
  = LitToken LitType
  | NameToken Name
  | LambdaToken Name
  | Block [Token]
  deriving (Show, Eq, Ord)

instance MP.ShowErrorComponent Token where
  showErrorComponent = show
```

The `Block` token covers expressions within parens.

The following parsers are defined to get tokens from a source file:

```haskell
tokensParser :: Parser [Token]
tokensParser = do
  MP.some $ do
    token <- tokenParser
    MPC.space
    pure token

tokenParser :: Parser Token
tokenParser =
      MP.try parensParser
  <|> MP.try litteralParser
  <|> MP.try referenceParser
  <|> lambdaParser

referenceParser :: Parser Token
referenceParser = do
  name <- nameParser
  pure $ NameToken name

--TODO: restrict the tokens used in nameParser.
nameParser :: Parser Name
nameParser = do
  firstLetter<- MPC.lowerChar
  rest <- MP.takeWhileP Nothing (\c -> c `notElem` ([' ', '\n', '(', ')']))
  pure $ Name (cons firstLetter rest)

lambdaParser :: Parser Token
lambdaParser = do
  MPC.char '\\'
  paramName <- nameParser
  MPC.string " ->"
  pure $ LambdaToken paramName

parensParser :: Parser Token
parensParser = do
  MPC.char '('
  tokens <- tokensParser
  MPC.char ')'
  pure $ Block tokens
```

Once parsing is done, we end up with a list of tokens. We want to convert that
list of tokens into a single expression.

Expressions:

```haskell
data Expression
  = Litteral LitType 
  | NameReference Name
  | Lambda Name Expression
  | Apply Expression Expression 
  deriving (Show, Eq, Ord)

instance MP.ShowErrorComponent Expression where
  showErrorComponent = show
```

A few similarities exist between tokens and expressions: name references,
lambdas and litterals.

There are two differences: `Token`s have `Block` and `Expression`s have
`Apply`. As we said earlier, there is no syntactic hint for `Apply`, which
is why it does not appear in tokens. `Block` is not needed to convey priority
in an Abstract Syntax Tree, so they don't appear in `Expression`.

The first step to transform a list of `Token` into an `Epression` is to give
`Lambda`s their right-hand side.

In order to do that, we introduce an intermediary form called `PartialExpression`,
which determines whether a `Token` can be evaluated to an `Expression` without
context, or whether more `Tokens` are needed to make an `Expression`.

```haskell
data PartialExpression
  = NoContext Expression
  | LambdaContext (Expression -> Expression)

instance Show PartialExpression where
  show (NoContext _) = "NoContext"
  show (LambdaContext _) = "LambdaContext"
```

`Block` tokens can be evaluated to an `Expression` by applying the algorithm
recursively. Therefore, only `LambdaTokens` generate `LambdaContext`:

```haskell
toLambdaPartial :: Token -> PartialExpression
toLambdaPartial (LitToken lt) = NoContext (Litteral lt)
toLambdaPartial (NameToken n) = NoContext (NameReference n)
toLambdaPartial (Block ts) = NoContext (toExpression ts)
toLambdaPartial (LambdaToken n) = LambdaContext (Lambda n)
```

Once we have transformed the list of `Token` into one of `PartialExpression`, we
need to elimilate `LambdaContext`s in orrder to have full `Expression`s remaining.

The right-hand side of a lambda (i.e. all the expressions further right from a
lambda in the `Expression` list) need to be fully evaluated in order to build
the lambda. This means that the lambda association operator (`->`) has lowest
priority.

```haskell
associateLambdas :: [PartialExpression] -> [Expression]
associateLambdas [] = []
associateLambdas (NoContext e : rest) =
  e : associateLambdas rest 
associateLambdas (LambdaContext f : rest) =
  [f (addApply (associateLambdas rest))]
```

Once all the lambdas have been associated to their right hand side, we have a
list of `Expression`. We can use the `Apply` expression to fold that list into a
single expression. 

As long as there is more than one token, we Apply the first token to the second,
getting one less token as a result.
So for a list `[a, b, c]` we will get `(Apply (Apply a b) c)`

```haskell
addApply :: [Expression] -> Expression
addApply [] = error "No expression was found"
addApply [e] = e
addApply (f:e:rest) =
  addApply ((Apply f e) : rest)
```

The `Token` to `Expression` pipeline is summed in the `toExpression` function.

```haskell
toExpression :: [Token] -> Expression
toExpression = addApply . associateLambdas . fmap toLambdaPartial

parseExpression :: Text -> Either Error Expression
parseExpression t =
  MP.runParser tokensParser "" t
  & fmap toExpression
  & mapLeft (\_ -> "")
```

Service code:

```haskell
testParser = MP.parseTest tokensParser
```
