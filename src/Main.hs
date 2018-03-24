{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (forever, forM_)
import Control.Monad.State.Lazy
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Eval as E
import qualified Parse as P
import System.Exit
import System.IO
import qualified System.Console.Haskeline as HL

data Memory = Memory { getHistory :: [(T.Text, E.EvalResult)] }

termSettings =
    HL.Settings {
        HL.complete = HL.completeFilename,
        HL.historyFile = Just "history.txt",
        HL.autoAddHistory = True
    }

addHistory :: (T.Text, E.EvalResult) -> StateT Memory IO ()
addHistory ln = do
    m <- get
    let h = getHistory m
    put $ Memory $ ln : h
    return ()

showHistoryLine :: (T.Text, E.EvalResult) -> StateT Memory IO ()
showHistoryLine hl = showLine $ (fst hl) <> " -> " <> (T.pack $ show (snd hl))

showLine :: T.Text -> StateT Memory IO ()
showLine = liftIO . TI.putStrLn

showHistory :: StateT Memory IO ()
showHistory = do
    m <- get
    let h = getHistory m
    forM_ h showHistoryLine

showPrompt :: StateT Memory IO ()
showPrompt = liftIO $ putStr "> "

getInputLine :: IO T.Text
getInputLine = do
    l <- HL.runInputT termSettings $ HL.getInputLine "> "
    case l of
        Nothing -> return ""
        Just s  -> return $ T.pack s

getNextLine :: StateT Memory IO T.Text
getNextLine = do
    ln <- liftIO getInputLine
    return $ T.strip ln

processExpressionLine :: T.Text -> StateT Memory IO ()
processExpressionLine l = do
    case P.parseExpression l of
        Left err   -> showLine $ P.mkParseErrorMessage l err
        Right expr ->
            let i  = E.eval expr
                is = T.pack $ show i
             in showLine l >> addHistory (l, i) >> showLine is

processLine :: T.Text -> StateT Memory IO ()
processLine t = do
    case t of
        "quit" -> liftIO (exitSuccess :: IO ())
        "list" -> showHistory
        ""     -> return ()
        e      -> processExpressionLine t

repl = getNextLine >>= processLine

main :: IO ()
main = do
    runStateT (forever repl) $ Memory []
    return ()
