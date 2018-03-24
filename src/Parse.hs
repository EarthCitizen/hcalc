module Parse (Parser, mkParseErrorMessage, parseExpression) where

import AST
import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Expr as ME
import qualified Text.Megaparsec.Char.Lexer as MCL

import Control.Monad.Combinators (between, skipCount)

type Parser = M.Parsec Void T.Text
type Error = M.ParseError Char Void

binaryl c t = ME.InfixL (c <$ symbol t)
binaryr c t = ME.InfixR (c <$ symbol t)

operators :: [[ME.Operator Parser Expression]]
operators = [[binaryr OperExp "^"]
            ,[binaryl OperMul "*"]
            ,[binaryl OperDiv "/"]
            ,[binaryl OperAdd "+"]
            ,[binaryl OperSub "-"]
            ]

space :: Parser ()
space = MC.space

symbol :: M.Tokens T.Text -> Parser (M.Tokens T.Text)
symbol = MCL.symbol space

discard :: M.Tokens T.Text -> Parser ()
discard t = () <$ symbol t

expression = ME.makeExprParser term operators

lexeme :: Parser p -> Parser p
lexeme p = MCL.lexeme space p

doNothing :: Parser ()
doNothing = return ()

parens :: Parser Expression
parens = between (symbol "(") (symbol ")") expression

float :: Parser Expression
float =
    let signed = MCL.signed doNothing MCL.float
     in lexeme (LitFloat <$> signed)

integer :: Parser Expression
integer =
    let signed = MCL.signed doNothing MCL.decimal
     in lexeme (LitInt <$> signed)

term :: Parser Expression
term = parens <|> (M.try float) <|> integer

mkParseErrorMessage :: T.Text -> Error -> T.Text
mkParseErrorMessage i e =
    let colNum  = M.unPos . M.sourceColumn . NE.head . M.errorPos $ e
        srcLine = i <> "\n"
        indLine = (T.replicate (colNum - 1) " ") <> "^\n"
        errMsg  = T.pack . M.parseErrorTextPretty $ e
     in srcLine <> indLine <> errMsg

parseExpression :: T.Text -> Either Error Expression
parseExpression t =
    let ep = expression <* M.eof
     in M.parse ep "" t
