module Parse (Parser, parseStmt) where

import Alias
import AST
import Control.Applicative (Alternative, (<|>))
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Void
import qualified Data.List.NonEmpty as NE
import Error (Error(ParseError))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Expr as ME
import qualified Text.Megaparsec.Char.Lexer as MCL

type Parser = M.Parsec Void String
type InternalParseError = M.ParseError Char Void

newtype ParserContext a = ParserContext { unParserContext :: ReaderT Source Parser a }
                        deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , Alternative
                                 , MonadPlus
                                 , M.MonadParsec Void String
                                 , MonadReader Source
                                 )

binaryl c t = ME.InfixL $ c <$> (location <* symbol t)
binaryr c t = ME.InfixR $ c <$> (location <* symbol t)

operators :: [[ME.Operator ParserContext Expr]]
operators = [[binaryr OperExp "^"]
            ,[binaryl OperMul "*"]
            ,[binaryl OperDiv "/"]
            ,[binaryl OperAdd "+"]
            ,[binaryl OperSub "-"]
            ]

space :: ParserContext ()
space = MC.space

space1 :: ParserContext ()
space1 = MC.space1

symbol :: M.Tokens String -> ParserContext (M.Tokens String)
symbol = MCL.symbol doNothing

discard :: M.Tokens String -> ParserContext ()
discard t = () <$ symbol t

lexeme :: ParserContext p -> ParserContext p
lexeme = MCL.lexeme space

lexeme1 :: ParserContext p -> ParserContext p
lexeme1 = MCL.lexeme space1

doNothing :: ParserContext ()
doNothing = return ()

nested :: ParserContext Expr
nested = M.between (symbol "(") (symbol ")") expression

float :: ParserContext Expr
float = LitNum <$> location <*> MCL.float

integer :: ParserContext Expr
integer = LitNum <$> location <*> (fromIntegral <$> MCL.decimal)

identifier :: ParserContext String
identifier = (:) <$> MC.letterChar <*> M.many MC.alphaNumChar

functionCall :: ParserContext Expr
functionCall = do
    let sf = space >> posNegFnParam
        ps = M.manyTill sf (M.notFollowedBy sf)
    FnCall <$> location <*> identifier <*> ps

functionParam :: ParserContext Expr
functionParam = let unary = (\l i -> FnCall l i []) <$> location <*> identifier
                 in nested <|> M.try unary <|> M.try float <|> M.try integer

posNegFnParam :: ParserContext Expr
posNegFnParam = M.try (negated functionParam) <|> functionParam

negated :: ParserContext Expr -> ParserContext Expr
negated p = Negate <$> location <*> (MC.char '-' *> p)

term :: ParserContext Expr
term = let p = nested <|> M.try functionCall <|> M.try float <|> M.try integer
        in lexeme p

posNegTerm :: ParserContext Expr
posNegTerm = M.try (negated term) <|> term

expression :: ParserContext Expr
expression = {-- M.dbg "debug" $ --} lexeme $ ME.makeExprParser (space *> posNegTerm) operators

statement :: ParserContext Stmt
statement = M.try stmtFnDef <|> (StmtExpr <$> location <*> expression)

stmtFnDef :: ParserContext Stmt
stmtFnDef = do
    (n:ps) <- M.sepEndBy1 identifier space1
    symbol "="
    StmtFnDef <$> location <*> (FnExpr n ps <$> expression)

location :: ParserContext Location
location = do
    src <- ask
    sp  <- M.getPosition
    let l = M.unPos . M.sourceLine   $ sp
        c = M.unPos . M.sourceColumn $ sp
    return (src, fromIntegral l, fromIntegral c)

ipeToParseErr :: InternalParseError -> Source -> Error
ipeToParseErr e s =
    let errPos = NE.head $ M.errorPos e
        linNum = fromIntegral . M.unPos $ M.sourceLine errPos
        colNum = fromIntegral . M.unPos $ M.sourceColumn errPos
        errMsg = M.parseErrorTextPretty e
     in ParseError (s, linNum, colNum) errMsg

parseStmt :: String -> Either Error Stmt
parseStmt s = let sp = statement <* M.eof
               in runParserContext sp s

runParserContext :: ParserContext a -> Source -> Either Error a
runParserContext pc s =
    case M.parse (runReaderT (unParserContext pc) s) "" s of
        Left ipe -> Left $ ipeToParseErr ipe s
        Right e  -> Right e

