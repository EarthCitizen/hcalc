module Session (runSession, runREPL) where

import Alias
import AST
import Control.Monad (forever)
import Control.Monad.Except(ExceptT(..), MonadError(..), liftEither, runExceptT)
import Control.Monad.Extra (eitherM)
import Control.Monad.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.List.Extra (trim)
import Error
import qualified Eval as E
import FnStore (GetFnStore(..), PutFnStore(..), putFnM)
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

newtype Session a = Session { unSession :: StateT Runtime (ExceptT Error (HL.InputT IO)) a }
                  deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadIO
                           , MonadError Error
                           , MonadState Runtime
                           , HL.MonadException
                           )

-- Modified version of instance for ErrorT that comes with haskeline
-- Haskeline currently does not include an instace for ExceptT
instance (HL.MonadException m) => HL.MonadException (ExceptT e m) where
    controlIO f = ExceptT $ HL.controlIO $ \(HL.RunIO run) ->
        let run' = HL.RunIO (fmap ExceptT . run . runExceptT)
         in fmap runExceptT $ f run'

instance GetFnStore Session where
    getFnStore = gets getStore

instance PutFnStore Session where
    putFnStore fs = modify $ \(Runtime h s) -> Runtime h fs

newSession :: Session ()
newSession = Session $ return ()

getPrompt :: Session String
getPrompt = Session $ lift $ lift $ p <$> HL.haveTerminalUI
    where p True  = "> "
          p False = ""

tryAction :: HL.InputT IO (Maybe String) -> HL.InputT IO (Maybe String)
tryAction f = HL.handle (\HL.Interrupt -> liftIO $ return $ Just "")
            $ HL.withInterrupt f

getSessionLine :: String -> Session (Maybe String)
getSessionLine prompt = Session $ lift $ lift $ tryAction $ HL.getInputLine prompt

processSessionLine :: Maybe String -> Session ()
processSessionLine = maybe exit go
    where go = processStmt . trim

processStmt :: String -> Session ()
processStmt "" = return ()
processStmt ln =
    let ev = \x -> do fs <- getFnStore
                      liftEither $ E.eval x fs
        pr = liftEither $ P.parseStmt ln
        ps (StmtFnDef _ fnDef) = putFnM fnDef
        ps (StmtExpr  _ expr)  = ev expr >>= showResult
     in pr >>= validate >>= ps

showError :: (MonadIO m) => Error -> m ()
showError err = showLines $ mkDetailedError err

showResult :: (MonadIO m) => Float50 -> m ()
showResult f = liftIO $ print f

showLines :: (MonadIO m) => [String] -> m ()
showLines = mapM_ showLine

showLine :: (MonadIO m) => String -> m ()
showLine s = liftIO $ putStrLn s

runREPL :: Session ()
runREPL = getPrompt >>= \p ->
    let processLine = getSessionLine p >>= processSessionLine
     in forever (catchError processLine showError)

runSession :: Session a -> IO ()
runSession s = let es = evalStateT (unSession s) mkDefaultRuntime
                   ex = runExceptT es
                   er = showError
                   ei = eitherM er (\_ -> return ()) ex
                in HL.runInputT termSettings ei

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

