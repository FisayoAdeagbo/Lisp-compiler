module Main where

import Parser
import Javascriptexpr
import Translator

main :: IO ()
main = getArgs >>= \case
  [file] ->
    printCompile =<< readFile file
  ["--e",file] ->
    either putStrLn print . runExprParser "--e" =<< readFile file
  ["--pp",file] ->
    either putStrLn (putStrLn . printExpr) . runExprParser "--pp" =<< readFile file
  ["--jse",file] ->
    either print (either putStrLn print . translateToJS) . runExprParser "--jse" =<< readFile file
  ["--ppc",file] ->
    either putStrLn (either putStrLn putStrLn) . fmap (compile . printExpr) . runExprParser "--ppc" =<< readFile file
  _ ->
    putStrLn $ unlines
      ["Usage: runghc Main.hs [ --e, --pp, --jse, --ppc ] <filename>"
      ,"--e     print the Expr"
      ,"--pp    pretty print Expr"
      ,"--jse   print the JSExpr"
      ,"--ppc   pretty print Expr and then compile"
      ]

printCompile :: String -> IO ()
printCompile = either putStrLn putStrLn . compile

compile :: String -> Either Error String
compile str = printJSExpr False 0 <$> (translateToJS =<< runExprParser "compile" str)