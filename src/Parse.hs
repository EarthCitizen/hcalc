module Parse (Parser, ParseError, mkParseErrorMessage, parseExpression) where

import AST
import Control.Applicative ((<|>), some)
import Data.Semigroup ((<>))
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Expr as ME
import qualified Text.Megaparsec.Char.Lexer as MCL

type Parser = M.Parsec Void String
type ParseError = M.ParseError Char Void

binaryl c t = ME.InfixL (c <$ symbol t)
binaryr c t = ME.InfixR (c <$ symbol t)

operators :: [[ME.Operator Parser Expr]]
operators = [[binaryr OperExp "^"]
            ,[binaryl OperMul "*"]
            ,[binaryl OperDiv "/"]
            ,[binaryl OperAdd "+"]
            ,[binaryl OperSub "-"]
            ]

space :: Parser ()
space = MC.space

space1 :: Parser ()
space1 = MC.space1

symbol :: M.Tokens String -> Parser (M.Tokens String)
symbol = MCL.symbol doNothing

discard :: M.Tokens String -> Parser ()
discard t = () <$ symbol t

lexeme :: Parser p -> Parser p
lexeme p = MCL.lexeme space p

lexeme1 :: Parser p -> Parser p
lexeme1 p = MCL.lexeme space1 p

doNothing :: Parser ()
doNothing = return ()

nested :: Parser Expr
nested = M.between (symbol "(") (symbol ")") expression

float :: Parser Expr
float = LitFloat <$> MCL.float

integer :: Parser Expr
integer = LitInt <$> MCL.decimal

identifier :: Parser String
identifier = (:) <$> MC.letterChar <*> M.many MC.alphaNumChar

functionCall :: Parser Expr
functionCall = do
    i  <- identifier
    let sf = (space1 >> posNegFnParam)
    ps <- M.manyTill sf (M.notFollowedBy sf)
    return $ FnCall i ps

functionParam :: Parser Expr
functionParam = let unary = ((\i -> FnCall i []) <$> identifier)
                 in nested <|> (M.try unary) <|> (M.try float) <|> (M.try integer)

posNegFnParam :: Parser Expr
posNegFnParam = (M.try $ negated functionParam) <|> functionParam

negated :: Parser Expr -> Parser Expr
negated p = Negate <$> (MC.char '-' *> p)

term :: Parser Expr
term = let p = nested <|> (M.try functionCall) <|> (M.try float) <|> (M.try integer)
        in lexeme p

posNegTerm :: Parser Expr
posNegTerm = (M.try $ negated term) <|> term

expression :: Parser Expr
expression = {-- M.dbg "debug" $ --} (lexeme $ ME.makeExprParser (space *> posNegTerm) operators)

mkParseErrorMessage :: ParseError -> String -> String
mkParseErrorMessage e l =
    let colNum  = M.unPos . M.sourceColumn . NE.head . M.errorPos $ e
        srcLine = l <> "\n"
        indLine = (replicate (colNum - 1) ' ') <> "^\n"
        errMsg  = M.parseErrorTextPretty $ e
     in srcLine <> indLine <> errMsg

parseExpression :: String -> Either ParseError Expr
parseExpression t = let ep = expression <* M.eof
                     in M.parse ep "" t
