module Session (runSession, sessionREPL) where

import AST
import Control.Monad (forever)
import Control.Monad.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.List.Extra (trim)
import Error
import qualified Eval as E
import FlexNum
import Runtime
import qualified Parse as P
import System.Exit
import qualified System.Console.Haskeline as HL
import Data.Maybe (maybe)
import Validation

exit = liftIO (exitSuccess :: IO ())

termSettings = HL.Settings {
                   HL.complete = HL.completeFilename,
                   HL.historyFile = Just "history.txt",
                   HL.autoAddHistory = True
               }

newtype Session a = Session { unSession :: StateT Runtime (HL.InputT IO) a }
                  deriving (Functor, Applicative, Monad, MonadIO, MonadState Runtime, HL.MonadException)

newSession :: Session ()
newSession = Session $ return ()

getPrompt :: Session String
getPrompt = Session $ lift $ p <$> HL.haveTerminalUI
    where p True  = "> "
          p False = ""

tryAction :: HL.InputT IO (Maybe String) -> HL.InputT IO (Maybe String)
tryAction f = HL.handle (\HL.Interrupt -> liftIO $ return $ Just "")
            $ HL.withInterrupt f

getSessionLine :: String -> Session (Maybe String)
getSessionLine prompt = Session $ lift $ tryAction $ HL.getInputLine prompt

processSessionLine :: Maybe String -> Session ()
processSessionLine = maybe exit go
    where go = processStmt . trim

processStmt :: String -> Session ()
processStmt "" = return ()
processStmt l  =
    case P.parseStmt l of
        Left err   -> showError l err
        Right (StmtFnDef _ fnDef) ->
            case validateFnDef fnDef of
                Left err -> showError l err
                Right _  -> addFunction fnDef
        Right (StmtExpr _ expr) -> do
            rt <- get
            let ef = E.eval expr $ getStore rt
            case ef of
                Left  err    -> showError l err
                Right result -> showResult result

showError :: String -> Error -> Session()
showError ln err = showLines $ mkDetailedError err

showResult :: FlexNum -> Session ()
showResult (FlexFloat f) = liftIO $ print f
showResult (FlexInt i)   = liftIO $ print i

showLines :: [String] -> Session ()
showLines = mapM_ showLine

showLine :: String -> Session ()
showLine s = liftIO $ putStrLn s

sessionREPL :: Session ()
sessionREPL = getPrompt >>= \p -> let process = getSessionLine p >>= processSessionLine
                                   in forever  process

runSession :: Session a -> IO a
runSession s = HL.runInputT termSettings $ evalStateT (unSession s) mkDefaultRuntime

-- addHistory :: (String, FlexNum) -> Session ()
-- addHistory ln = do
--     modify $ \m -> Memory $ ln : getHistory m
--     return ()

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
