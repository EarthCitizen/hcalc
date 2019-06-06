module Test.Util.GenParse (genStmts) where

import Alias
import AST
import FnStore
import Test.Util.Data

import Control.Monad.ListM (intercalateM)
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genList :: (MonadGen m) => m a -> m [a]
genList = Gen.list (Range.linear 0 200)

genFnName :: (MonadGen m) => m String
genFnName = do
    c1 <- Gen.alpha
    cx <- Gen.list (Range.linear 0 200) Gen.alphaNum
    return (c1:cx)

genFloat :: (MonadGen m) => m Float50
genFloat = Gen.realFrac_ $ Range.constant 1 1000

genExpr :: (MonadGen m) => m Expr
genExpr = do
    let nr = [ litNum_ <$> genFloat
             , negate_ . litNum_ <$> genFloat
             ]
        rc = [ operExp_ <$> genExpr   <*> genExpr
             , operMul_ <$> genExpr   <*> genExpr
             , operDiv_ <$> genExpr   <*> genExpr
             , operAdd_ <$> genExpr   <*> genExpr
             , operSub_ <$> genExpr   <*> genExpr
             , fnCall_  <$> genFnName <*> (genList genExpr)
             ]
        ng = (fmap . fmap ) negate_ rc
    Gen.recursive Gen.choice nr (rc ++ ng)

genFnDef :: (MonadGen m) => m FnDef
genFnDef = FnExpr <$> genFnName <*> (genList genFnName) <*> genExpr

genStmt :: (MonadGen m) => m Stmt
genStmt = Gen.choice [ stmtFnDef_ <$> genFnDef
                     , stmtExpr_  <$> genExpr
                     ]

genStmts :: (MonadGen m) => m [Stmt]
genStmts = genList genStmt

