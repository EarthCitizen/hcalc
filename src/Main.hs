{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (forever, forM_, join)
import Control.Monad.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.List.Extra (trim)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Eval as E
import FlexNum
import qualified Parse as P
import System.Exit
import System.IO
import qualified System.Console.Haskeline as HL
import Data.Maybe (maybe)

exit = liftIO (exitSuccess :: IO ())

termSettings = HL.Settings {
                   HL.complete = HL.completeFilename,
                   HL.historyFile = Just "history.txt",
                   HL.autoAddHistory = True
               }

data Memory = Memory { getHistory :: [(String, FlexNum)] } deriving (Show)

newtype Session a = Session { unSession :: StateT Memory (HL.InputT IO) a }
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState Memory, HL.MonadException)

newSession :: Session ()
newSession = Session $ return ()

getPrompt :: Session (String)
getPrompt = Session $ lift $ p <$> HL.haveTerminalUI
    where p True  = "> "
          p False = ""

tryAction :: HL.InputT IO (Maybe String) -> HL.InputT IO (Maybe String)
tryAction f = HL.handle (\HL.Interrupt -> liftIO $ (return (Just "")))
            $ HL.withInterrupt $ f

getSessionLine :: String -> Session (Maybe String)
getSessionLine prompt = Session $ lift $ tryAction $ HL.getInputLine prompt

processSessionLine :: Maybe String -> Session ()
processSessionLine m = maybe exit go m
    where go = processExpression . trim

processExpression :: String -> Session ()
processExpression "" = return ()
processExpression l  =
    case P.parseExpression l of
        Left err   -> showParseError err l
        Right expr -> let n = E.eval expr
                       in addHistory (l, n) >> showResult n

showParseError :: P.ParseError -> String -> Session ()
showParseError e l = showLine $ P.mkParseErrorMessage e l

showResult :: FlexNum -> Session ()
showResult (FlexFloat f) = liftIO $ putStrLn $ show f
showResult (FlexInt i)   = liftIO $ putStrLn $ show i

showLine :: String -> Session ()
showLine s = liftIO $ putStrLn s

sessionREPL :: Session ()
sessionREPL = getPrompt >>= \p -> let process = getSessionLine p >>= processSessionLine
                                   in forever  process

runSession :: Session a -> IO a
runSession s = HL.runInputT termSettings $ (evalStateT (unSession s) (Memory []))

addHistory :: (String, FlexNum) -> Session ()
addHistory ln = do
    modify $ \m -> Memory $ ln : getHistory m
    return ()

-- showHistoryLine :: (T.Text, E.EvalResult) -> StateT Memory IO ()
-- showHistoryLine hl = showLine $ (fst hl) <> " -> " <> (T.pack $ show (snd hl))

-- showHistory :: StateT Memory IO ()
-- showHistory = do
--     m <- get
--     let h = getHistory m
--     forM_ h showHistoryLine
--
-- processREPLLine :: T.Text -> StateT Memory IO ()
-- processREPLLine t = do
--     case t of
--         "quit" -> liftIO (exitSuccess :: IO ())
--         "list" -> showHistory
--         ""     -> return ()
--         e      -> processExpressionLine t

main :: IO ()
main = do
    runSession sessionREPL
