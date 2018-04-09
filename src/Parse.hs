module Parse (Parser, parseExpression) where

import Alias
import AST
import Control.Applicative ((<|>), some)
import Data.Semigroup ((<>))
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Error (Error(ParseError))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Expr as ME
import qualified Text.Megaparsec.Char.Lexer as MCL

type Parser = M.Parsec Void String
type InternalParseError = M.ParseError Char Void

binaryl c t = ME.InfixL $ c <$> (location <* symbol t)
binaryr c t = ME.InfixR $ c <$> (location <* symbol t)

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
float = LitFloat <$> location <*> MCL.float

integer :: Parser Expr
integer = LitInt <$> location <*> MCL.decimal

identifier :: Parser String
identifier = (:) <$> MC.letterChar <*> M.many MC.alphaNumChar

functionCall :: Parser Expr
functionCall = do
    let sf = (space1 >> posNegFnParam)
        ps = M.manyTill sf (M.notFollowedBy sf)
    FnCall <$> location <*> identifier <*> ps

functionParam :: Parser Expr
functionParam = let unary = (\l i -> FnCall l i []) <$> location <*> identifier
                 in nested <|> (M.try unary) <|> (M.try float) <|> (M.try integer)

posNegFnParam :: Parser Expr
posNegFnParam = (M.try $ negated functionParam) <|> functionParam

negated :: Parser Expr -> Parser Expr
negated p = Negate <$> location <*> (MC.char '-' *> p)

term :: Parser Expr
term = let p = nested <|> (M.try functionCall) <|> (M.try float) <|> (M.try integer)
        in lexeme p

posNegTerm :: Parser Expr
posNegTerm = (M.try $ negated term) <|> term

expression :: Parser Expr
expression = {-- M.dbg "debug" $ --} (lexeme $ ME.makeExprParser (space *> posNegTerm) operators)

location :: Parser Location
location = do
    sp <- M.getPosition
    let l = M.unPos . M.sourceLine   $ sp
        c = M.unPos . M.sourceColumn $ sp
    return (fromIntegral l, fromIntegral c)

ipeToParseErr :: InternalParseError -> Error
ipeToParseErr e =
    let errPos = NE.head $ M.errorPos e
        linNum = fromIntegral . M.unPos $ M.sourceLine errPos
        colNum = fromIntegral . M.unPos $ M.sourceColumn errPos
        errMsg = M.parseErrorTextPretty $ e
     in ParseError (linNum, colNum) errMsg

parseExpression :: String -> Either Error Expr
parseExpression t = let ep = expression <* M.eof
                     in case M.parse ep "" t of
                         Left ipe -> Left $ ipeToParseErr ipe
                         Right e  -> Right e
