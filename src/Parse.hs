module Parse (Parser, parseLine, parseStmt) where

import Alias
import AST
import Control.Applicative (Alternative, (<|>))
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Function ((&))
import Data.List (sortBy)
import Data.List.Extra (trim, wordsBy)
import Data.Void
import Error (Error(ParseError))
import Predef (predefOprsByPrec)

import qualified Data.Map.Strict as MAP
import qualified Data.List.NonEmpty as NE
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

ordInvert LT = GT
ordInvert EQ = EQ
ordInvert GT = LT

highestFirst = \x y -> compare x y & ordInvert

precs = MAP.keys predefOprsByPrec & sortBy highestFirst

operToParser :: Operator -> ME.Operator ParserContext Expr
operToParser o@(OpInfix _ fx _ fn) =
    f fx $ OperInf <$> location <*> pure o <* (symbol $ operString o)
    where f LFix = ME.InfixL
          f RFix = ME.InfixR

operators :: [[ME.Operator ParserContext Expr]]
operators = let opPrecLists = (\k -> (MAP.!) predefOprsByPrec k) <$> precs
                ff = fmap . fmap
             in ff operToParser opPrecLists
-- operators = MAP.elems $ MAP.map (fmap operToParser) predefOprsByPrec

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
    let sf = space >> functionParam
        ps = M.manyTill sf (M.notFollowedBy sf)
    FnCall <$> location <*> identifier <*> ps

functionParam :: ParserContext Expr
functionParam = let nullary = (\l i -> FnCall l i []) <$> location <*> identifier
                 in nested <|> M.try nullary <|> M.try float <|> M.try integer

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
    fnps    <- M.sepEndBy1 identifier space1
    (n, ps) <- failIfEmpty fnps
    symbol "="
    StmtFnDef <$> location <*> (FnExpr n ps <$> expression)
        -- This is only to make GHC 8.6 happy
        -- Without this, it complains that the pattern match,
        -- which was previously (n:ps) would fail:
        -- > No instance for (Control.Monad.Fail.MonadFail ParserContext)
        -- > arising from a do statement
        -- > with the failable pattern ‘(n : ps)’
        where failIfEmpty [] = fail "This should never happen"
              failIfEmpty (n:ps) = return (n, ps)


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
parseStmt s = let sp = space *> statement <* M.eof
               in runParserContext sp s

parseLine :: String -> Either Error [Stmt]
parseLine s = let stmts = filter (not . null . trim) $ wordsBy (==';') s
               in forM stmts parseStmt

runParserContext :: ParserContext a -> Source -> Either Error a
runParserContext pc s =
    case M.parse (runReaderT (unParserContext pc) s) "" s of
        Left ipe -> Left $ ipeToParseErr ipe s
        Right e  -> Right e

