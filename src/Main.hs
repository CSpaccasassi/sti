module Main where

import Parser
import Types
import AlgorithmF
import AlgorithmR
import AlgorithmW 
import AlgorithmST
import AlgorithmD
import Text.PrettyPrint.HughesPJ ( render, text, (<>) ) 
import System.Environment
import System.Console.GetOpt
import Data.List
import PP.Literal

s6 :: String
s6  = "let p = req-c () in send (p, 1); sel-ADD p "

------------------------------------
-- COMMAND LINE SETUP
------------------------------------

version :: Double
version = 0.1

-- the cases of the command-line arguments
data Flag = Help 
          | Version 
          | PrintLit -- print output in literal terms (Thea's format)
          -- selectable outputs to print
          | PrintBehaviour
          | PrintConstraints
          | PrintExpression
          | PrintType 
          -- file options
          | IFile String
          | OFile String
  deriving Eq

-- the syntax of command-line arguments
options :: [OptDescr Flag]
options = [
  -- info
  Option "?" ["help"]     (NoArg Help)      "shows help",
  Option "v" ["version"]  (NoArg Version)   "shows version number and exit",
  -- output kinds
  Option "l" ["literal"]  (NoArg PrintLit)   "all outputs are in literal format (default: latex)",
  -- output
  Option "b" ["behaviours"] (NoArg PrintBehaviour) 
        "print inferred behaviour (default is print all inferred information)",
  Option "c" ["constraints"] (NoArg PrintConstraints)
      "print inferred constraints (default is print all inferred information)",
  Option "e" ["expression"] (NoArg PrintExpression)
      "print annotated expression (default is print all inferred information)",
  Option "t" ["type"] (NoArg PrintType)
      "print inferred constraints (default is print all inferred information)",
          
  -- input
  Option "f" ["file"]   (ReqArg IFile "FILE")  
                          "reads source from FILE (default is standard input)",
  Option "o" ["output"] (ReqArg OFile "FILE")   
              "outputs inferred information to FILE (default is standard ouput)"                     
  ]

-- find a filename in a flag or else return 2nd argument
findIFile :: Flag -> String -> String
findIFile flag fname = case flag of
                         IFile file -> file
                         _          -> fname

-- find a filename in a flag or else return 2nd argument
findOFile :: Flag -> String -> String
findOFile flag fname = case flag of
                         OFile file -> file
                         _          -> fname

run :: [Flag] -> IO()
run flags
  -- print Help info
  | Help `elem` flags = do 
     progname <- getProgName
     putStr $ usageInfo ("Usage: " ++ progname ++ " [OPTION...]") options
  
  -- just print version and exit
  | Version `elem` flags =  
     putStr $ "Version: " ++ show version        
  
  -- run the interpreter with a timeout
  | otherwise = do                            
      
      -- configure run      
      let ifilename = foldr findIFile "" flags  -- get file name in the flags
      code <- if length ifilename > 0       
        then readFile ifilename
        else getContents
      e <- parser "" code
      (_, t1, b1, c1) <- algorithmW [] e
--      (_, c2) <- algorithmST b1 c1
--      let (_, c3) = algorithmD c2
--      (_, c4) <- algorithmF c3
--      let (c5, _, _) = algorithmR [] (c4, TUnit, BTau)
      
--      putStrLn $ "final result: " ++ (render . ppConstraints) c5
      
      -- configure output
      let isLit  = PrintLit `elem` flags
          ps     = [PrintExpression, PrintType, PrintBehaviour, PrintConstraints]
          pflags = if null (ps `intersect` flags)
                    then ps  -- default: print all inferred information
                    else ps `intersect` flags -- print selected info only
          docs = map (\p -> case p of 
              PrintExpression -> text ("\\\\Expression: $ " ++ show e ++ "$\n")
              PrintType       -> text ("\\\\Type: $ "       ++ show t1 ++ "$\n")
              PrintBehaviour  -> if isLit
                                  then ppBLit b1 <> text "\n\n"
                                  else text ("\\\\Behaviour: $ "  ++ show b1 ++ "$\n")
              PrintConstraints-> if isLit
                                  then ppConstraintsLit c1
                                  else text "\\\\Constraints:" <> ppConstraints c1
              _ -> error "unexpected flag"         
            ) pflags
            
          doc = render $ foldl (<>) (text "") docs
      
      let ofilename = foldr findOFile "" flags  -- get file name in the flags
--      putStrLn $ "Simplified behaviour:" ++ show (simplifyBehaviour c1 b1)
      if length ofilename > 0       
        then writeFile ofilename doc
        else putStrLn doc
      
--      putStrLn "Inference complete."
--      putStr "\\\\Expression: $ "
--      putStrLn $ show e ++ "$ "
--      putStr "\\\\Type: $" 
--      putStrLn $ show t ++ "$ "
--      putStr "\\\\Behaviour: $"
--      putStrLn $ show b ++ "$ "
--      --  putStr "\\\\Substitution: "
--      --  print s 
--      putStrLn "\\\\Constraints:"
--      putStrLn $ (render . ppConstraints) cs               

main:: IO ()
--main = infer "let p1 = req-c () in let p2 = req-d () in (p1, p2)"
main = do
  progname  <- getProgName
  args      <- getArgs
  let header = "Usage: " ++ progname ++ " [OPTION...]"
  let usage  = usageInfo header options
  case getOpt RequireOrder options args of
    (flags, [],      [])  -> run flags
    (_,     nonOpts, [])  -> error $ "unrecognized arguments: " 
                                  ++ unwords nonOpts ++ "\n" 
                                  ++ usage
    (_,     _,       err) -> error $ concat err ++ usage

 