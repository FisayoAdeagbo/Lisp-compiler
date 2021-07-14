module Javascriptexpr where

import Parser

data JSExpr
    = JSInt Int
    | JSSymbol Name
    | JSBinOp JSBinOp JSExpr JSExpr
    | JSLambda [Name] JSExpr
    | JSFunCall JSExpr [JSExpr]
    | JSReturn JSExpr
    deriving (Eq, Show, Read)

type JSBinOp = String

printJSOp :: JSBinOp -> String
printJSOp op = op

printJSExpr :: Bool -> Int -> JSExpr -> String
printJSExpr doindent tabs = \case
  JSInt    i     -> show i
  JSSymbol name  -> name
  JSLambda vars expr -> (if doindent then indent tabs else id) $ unlines
    ["function(" ++ intercalate ", " vars ++ ") {"
    ,indent (tabs+1) $ printJSExpr False (tabs+1) expr
    ] ++ indent tabs "}"
  JSBinOp  op e1 e2  -> "(" ++ printJSExpr False tabs e1 ++ " " ++ printJSOp op ++ " " ++ printJSExpr False tabs e2 ++ ")"
  JSFunCall f exprs  -> "(" ++ printJSExpr False tabs f ++ ")(" ++ intercalate ", " (fmap (printJSExpr False tabs) exprs) ++ ")"
  JSReturn expr      -> (if doindent then indent tabs else id) $ "return " ++ printJSExpr False tabs expr ++ ";"
