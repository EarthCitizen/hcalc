module Predef ( predefFns
              , predefOprs
              , predefOprsByChars
              , predefOprsByPrec
              , predefOperExp
              , predefOperMul
              , predefOperDiv
              , predefOperAdd
              , predefOperSub
              , predefStrExp
              , predefStrMul
              , predefStrDiv
              , predefStrAdd
              , predefStrSub
              ) where

import AST

import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict    as M

predefFns :: [FnDef]
predefFns = [ FnReal "pi"    []    (FnNullary pi)
            , FnReal "sqrt"  ["x"] (FnUnary sqrt)
            , FnReal "acos"  ["x"] (FnUnary acos)
            , FnReal "asin"  ["x"] (FnUnary asin)
            , FnReal "atan"  ["x"] (FnUnary atan)
            , FnReal "cos"   ["x"] (FnUnary cos)
            , FnReal "sin"   ["x"] (FnUnary sin)
            , FnReal "tan"   ["x"] (FnUnary tan)
            , FnReal "acosh" ["x"] (FnUnary acosh)
            , FnReal "asinh" ["x"] (FnUnary asinh)
            , FnReal "atanh" ["x"] (FnUnary atanh)
            , FnReal "cosh"  ["x"] (FnUnary cosh)
            , FnReal "sinh"  ["x"] (FnUnary sinh)
            , FnReal "tanh"  ["x"] (FnUnary tanh)
            , FnReal "exp"   ["x"] (FnUnary exp)
            , FnReal "log"   ["x"] (FnUnary log)
            , FnReal "tst"   ["a", "b"] (FnBinary (+))
            ]

predefStrExp = N.fromList "^"
predefStrMul = N.fromList "*"
predefStrDiv = N.fromList "/"
predefStrAdd = N.fromList "+"
predefStrSub = N.fromList "-"

predefOperExp = OpInfix { chars = predefStrExp, fixity = RFix, prec = 100, fn = (**) }
predefOperMul = OpInfix { chars = predefStrMul, fixity = LFix, prec = 75,  fn = (*)  }
predefOperDiv = OpInfix { chars = predefStrDiv, fixity = LFix, prec = 75,  fn = (/)  }
predefOperAdd = OpInfix { chars = predefStrAdd, fixity = LFix, prec = 25,  fn = (+)  }
predefOperSub = OpInfix { chars = predefStrSub, fixity = LFix, prec = 25,  fn = (-)  }

predefOprs :: [Operator]
predefOprs = [predefOperExp, predefOperMul, predefOperDiv, predefOperAdd, predefOperSub]

predefOprsByChars :: M.Map String Operator
predefOprsByChars = foldl (\m o -> M.insert (operString o) o m) M.empty predefOprs

predefOprsByPrec :: M.Map Int [Operator]
predefOprsByPrec = let ts = (\o -> (prec o, o)) <$> predefOprs
                       f  = \m (p, o) -> M.insertWith (++) p [o] m
                    in foldl f M.empty ts

