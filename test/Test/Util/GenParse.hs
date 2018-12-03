module Test.Util.GenParse where

import Alias
import AST
import FnStore
import Test.Util.Data

import Control.Monad.ListM (intercalateM)
import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genList :: (MonadGen m) => m a -> m [a]
genList = Gen.list (Range.constant 0 10)

genFnName :: (MonadGen m) => m String
genFnName = do
    c1 <- Gen.alpha
    cx <- Gen.list (Range.constant 0 50) Gen.alphaNum
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
    Gen.recursive Gen.choice nr rc

genFnDef :: (MonadGen m) => m FnDef
genFnDef = FnExpr <$> genFnName <*> (genList genFnName) <*> genExpr

genStmt :: (MonadGen m) => m Stmt
genStmt = Gen.choice [ stmtFnDef_ <$> genFnDef
                     , stmtExpr_  <$> genExpr
                     ]

genSpacing :: (MonadGen m) => m String
genSpacing = let r = Range.constant 1 12
                 a = Gen.list r $ Gen.element [' ', '\t']
                 b = Gen.constant []
              in Gen.frequency [ (1, a)
                               , (2, b)
                               ]

genSemicolonList :: (MonadGen m) => m String
genSemicolonList = do
    let r = Range.constant 1 50
    scs <- Gen.frequency [ (1, Gen.constant [";"])
                         , (2, Gen.list r $ Gen.constant ";")
                         ]
    bs  <- genSpacing
    is  <- intercalateM genSpacing scs
    as  <- genSpacing
    return $ bs ++ is ++ as

genConcatPad :: (MonadGen m ) => [String] -> m String
genConcatPad ss = do
    bs <- genSpacing
    is <- intercalateM genSpacing ss
    as <- genSpacing
    return $ concat [bs, is, as]

