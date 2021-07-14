module Translator where

import Javascriptexpr

type TransError = String

translateToJs :: Expr -> Either TransError JSExpr
translateToJs = \case
    ATOM (Symbol s) -> pure $ JSSymbol s
    ATOM (Int i)    -> pure $ JSInt i
    LIST xs         -> translateList xs

translateList :: [Expr] -> Either TransError JSExpr
translateList = \case
    []                 -> Left "empty list"
    ATOM (Symbol s):xs
        | Just f <- lookup s builtins ->
             f xs
    f:xs ->
        JSFunCall <$> translateToJS f <*> traverse translateToJS xs


type Builtin = [Expr] -> Either TransError JSExpr
type Builtins = [(Name, Builtin)]

builtins :: Builtins
builtins =
    [("lambda", transLambda)
  ,("let", transLet)
  ,("add", transBinOp "add" "+")
  ,("mul", transBinOp "mul" "*")
  ,("sub", transBinOp "sub" "-")
  ,("div", transBinOp "div" "/")
  ,("print", transPrint)
  ]

transLambda :: [Expr] -> Either TransError JSExpr
transLambda =  \case
    [LIST vars, body] -> do
        vars' <- traverse fromSymbol vars
        JSLambda vars' <$> (JSReturn <$> translateToJS body)

    vars ->
        Left $ unlines
         ["Syntax error: unexpected arguments for lambda."
         ,"expecting 2 arguments, the first is the list of vars and the second is the body of the lambda."
         ,"In expression: " ++ show (LIST $ ATOM (Symbol "lambda") : vars)
         ]

fromSymbol :: Expr -> Either String Name
fromSymbol (ATOM (Symbol s)) = Right s
fromSymbol e = Left $ "Not a symbol " ++ show e

transLet :: [Expr] -> Either TransError JSExpr
transLet = \case
  [LIST binds, body] -> do
    (vars, vals) <- letParams binds
    vars' <- traverse fromSymbol vars
    JSFunCall . JSLambda vars' <$> (JSReturn <$> translateToJS body) <*> traverse translateToJS vals
   where
    letParams :: [Expr] -> Either Error ([Expr],[Expr])
    letParams = \case
      [] -> pure ([],[])
      LIST [x,y] : rest -> ((x:) *** (y:)) <$> letParams rest
      x : _ -> Left ("Unexpected argument in let list in expression:\n" ++ printExpr x)


