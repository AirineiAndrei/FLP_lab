
module Parsing where

import Exp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle, LanguageDef )
import Text.ParserCombinators.Parsec.Token
import Control.Applicative (some)

miniHaskellDef :: LanguageDef st
miniHaskellDef = haskellStyle {
                                reservedOpNames= ["\\", "->", "=",":="],
                                reservedNames  = ["let", "letrec", "in"]
                              }

miniHs :: TokenParser st
miniHs = makeTokenParser miniHaskellDef

testParse :: Parser a -> String -> a
testParse p s
  = case parse p "<input>" s of
      Left err -> error (show err)
      Right a -> a

var :: Parser Var
var = do
  name <- identifier miniHs
  return (Var name)
-- >>> testParse var "b is a var"
-- Var {getVar = "b"}

varExp :: Parser ComplexExp
varExp = do
    name <- identifier miniHs
    return (CX (Var name))
-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})

lambdaExp :: Parser ComplexExp
lambdaExp = do
            _ <- reservedOp miniHs "\\"
            inLmabda <- identifier miniHs
            spaces
            _ <- reservedOp miniHs "->"
            spaces
            outLambda <- expr
            return (CLam (Var inLmabda) (outLambda))
-- >>> testParse lambdaExp "\\x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))

letExp :: Parser ComplexExp
letExp = do
            _ <- reserved miniHs "let"
            spaces
            x <- identifier miniHs
            spaces
            _ <- reservedOp miniHs ":="
            spaces
            y <- expr
            spaces
            _ <- reserved miniHs "in"
            spaces
            z <- expr

            return (Let (Var x) (y) (z))
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

letrecExp :: Parser ComplexExp
letrecExp = do
            _ <- reserved miniHs "letrec"
            spaces
            x <- identifier miniHs
            spaces
            _ <- reservedOp miniHs ":="
            spaces
            y <- expr
            spaces
            _ <- reserved miniHs "in"
            spaces
            z <- expr

            return (LetRec (Var x) (y) (z))
-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

listExp :: Parser ComplexExp
listExp = do
            _ <- reservedOp miniHs "["
            vals <- sepBy (identifier miniHs) (char ',')
            _ <- reserved miniHs "]"
            return (List (map CX (map Var vals)))

-- >>> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

natExp :: Parser ComplexExp
natExp = undefined
-- >>> testParse natExp "223 a"
-- Nat 223

parenExp :: Parser ComplexExp
parenExp = undefined
-- >>> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})

basicExp :: Parser ComplexExp
basicExp = undefined
-- >>> testParse basicExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

expr :: Parser ComplexExp
expr = varExp
-- >>> testParse expr "\\x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])

exprParser :: Parser ComplexExp
exprParser = whiteSpace miniHs *> expr <* eof
-- >>> testParse exprParser "let x := 28 in \\y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))

