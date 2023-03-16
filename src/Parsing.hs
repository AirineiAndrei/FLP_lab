
module Parsing where

import Exp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle, LanguageDef )
import Text.ParserCombinators.Parsec.Token
import Control.Applicative (some)

miniHaskellDef :: LanguageDef st
miniHaskellDef = haskellStyle {
                                reservedOpNames= ["\\", "->", "=",":=","+"],
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
    varr <- var
    return (CX varr)
-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})

lambdaExp :: Parser ComplexExp
lambdaExp = do
            _ <- reservedOp miniHs "\\"
            inLmabda <- var
            _ <- reservedOp miniHs "->"
            outLambda <- expr
            return (CLam inLmabda (outLambda))
-- >>> testParse lambdaExp "\\x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))

letExp :: Parser ComplexExp
letExp = do
            _ <- reserved miniHs "let"
            x <- var
            _ <- reservedOp miniHs ":="
            y <- expr
            _ <- reserved miniHs "in"
            z <- expr
            return (Let x y z)
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

letrecExp :: Parser ComplexExp
letrecExp = do
            _ <- reserved miniHs "letrec"
            x <- var
            _ <- reservedOp miniHs ":="
            y <- expr
            _ <- reserved miniHs "in"
            z <- expr
            return (LetRec x y z)
-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))
listExp :: Parser ComplexExp
listExp = do
  exprs <- brackets miniHs (commaSep miniHs expr)
  return (List exprs)

-- >>> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

natExp :: Parser ComplexExp
natExp = do
          val <- natural miniHs
          return (Nat (fromIntegral val))
-- >>> testParse natExp "223 a"
-- Nat 223

parenExp :: Parser ComplexExp
parenExp = do
          val <- parens miniHs expr
          return val
-- >>> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})

opExp :: Parser ComplexExp
opExp = do
          _ <- reservedOp miniHs "+"
          return (CX (Var "+"))

basicExp :: Parser ComplexExp
basicExp = varExp <|> lambdaExp <|> letExp <|> letrecExp <|> listExp <|> natExp <|> parenExp <|> opExp
-- >>> testParse basicExp "[a,b,c]" 
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

expr :: Parser ComplexExp
expr = do
          exps <- some basicExp
          return (foldl1 CApp exps)

-- >>> testParse expr "\\x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])

exprParser :: Parser ComplexExp
exprParser = whiteSpace miniHs *> expr <* eof
-- >>> testParse exprParser "let x := 28 in \\y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))

