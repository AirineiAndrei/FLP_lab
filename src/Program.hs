
module Program where
import Exp
import Lab2 ( Parser, endOfInput, whiteSpace, reserved, semiSep1,semi )
import Parsing ( expr, var, parseFirst )
import Sugar ( desugarExp, desugarVar )
import Eval ( substitute , normalize)

import Control.Applicative ( Alternative(..) )
import System.IO ( stderr, hPutStrLn )
import qualified Data.Map.Strict as Map

data Definition = Definition
  { defHead :: Var
  , defArgs :: [Var]
  , defBody :: ComplexExp
  }
  deriving (Show)

definition :: Parser Definition
definition = do
  headVar <- var
  args <- many var
  body <- reserved ":=" >> expr
  return $ Definition headVar args body

-- >>> parseFirst definition "id := \\x -> x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})

-- >>> parseFirst definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})

-- >>> parseFirst definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})

program :: Parser [Definition]
program = do
  whiteSpace
  lines <- many (definition <* semi)
  endOfInput
  return lines

-- >>> parseFirst program "    id x := x ; const x y := x"
-- Nothing

-- >>> parseFirst program "    id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

definitionExp :: Definition -> ComplexExp
definitionExp (Definition headVar [] bodyExp) = bodyExp
definitionExp (Definition headVar (headArgs:leftArgs) bodyExp) = CLam headArgs (definitionExp (Definition headVar leftArgs bodyExp))

-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar = "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv pgm = Map.fromList (zip (map desugarVar(map defHead pgm)) (map desugarExp (map definitionExp pgm)))

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env exp  = maybe exp (normalizeEnv env) (step exp)
  where
    step (X x) = Map.lookup x env
    step (Lam x e) = Lam x <$> step e
    step (App (Lam x ex) e) = Just (substitute x e ex)
    step (App e1 e2)
      = case step e1 of
        Nothing -> App e1 <$> step e2
        Just e1' -> Just (App e1' e2)

