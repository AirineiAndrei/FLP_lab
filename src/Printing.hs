
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar var = getVar var

showExp :: ComplexExp -> String
showExp (CX var) = showVar var
showExp (Nat nat) = show nat
showExp (CLam inLmabda outLambda) = "\\" ++ (showVar inLmabda) ++ " -> " ++ (showExp outLambda)
showExp (CApp exp1 exp2) = (showExp exp1) ++ " " ++ (showExp exp2)
showExp (Let x val exp) = "let " ++ (showVar x) ++ " = " ++  (showExp val) ++ " in " ++ (showExp exp)
showExp (LetRec x val exp) = "letrec " ++ (showVar x) ++ " = " ++  (showExp val) ++ " in " ++ (showExp exp)
showExp (List vals) = (show (map showExp vals))
showExp _ = "undefined"

