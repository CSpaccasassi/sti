module Parser (parser) where

import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim (ParsecT, parse, (<|>), (<?>))

import Data.Functor.Identity
import Syntax
import Data.Unique (newUnique)
--import Debug.Trace (trace)

-------------------
-- Parser
-------------------

-- Lexer & Parser
tcmlStyle :: LanguageDef st
tcmlStyle = haskellStyle
              {
                reservedOpNames= ["=", "\\", "->", "<=", "==", "||",
                                  "req-", "acc-", "sel-", ":", ";",
                                  "[", "|>", "]", ","],
                reservedNames  = [
                    "ff", "tt", "()", "fst", "snd", 
                    "let", "in", "if", "then", "else", "fix", "fun", 
                    "request", "accept", "send", "recv", "deleg", "resume",
                    "select", "match", "with", "spawn" ] 
              }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser tcmlStyle

whiteSpace :: Text.Parsec.Prim.ParsecT String () 
                Data.Functor.Identity.Identity ()
whiteSpace = T.whiteSpace lexer

braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = T.braces lexer

parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens     = T.parens lexer

identifier :: ParsecT String () Identity String
identifier = T.identifier lexer

reserved :: String -> ParsecT String () Identity ()
reserved = T.reserved lexer

reservedOp :: String -> ParsecT String () Identity ()
reservedOp = T.reservedOp lexer

integer :: ParsecT String () Identity Integer
integer = T.integer lexer



-- parse value
pValue :: P.Parser Val
pValue = do
          _   <- reserved "fun"
          x   <- identifier
          _   <- reservedOp "=>"
          _   <- whiteSpace
          e   <- pTerm
          return ( Fun x ( abstract 0 x e ) )
     <|>  do
          _   <- reserved "fix"
          f   <- identifier
          x   <- parens identifier
          _   <- reservedOp "=>"
          e   <- pTerm
          return ( Fix f x ( abstract 1 x  ( abstract 0 f e ) ) )
     <|> do
         _ <- reserved "()"
         return U
     <|> do
         _ <- reserved "tt"
         return TT
     <|> do
         _ <- reserved "ff"
         return FF
     <|>
        do
          n <- integer
          return (N n)
     <?> "Expecting a value"
  

-- parse constant
pConstant :: P.Parser Constant
pConstant = do
      _ <- reserved "send"
      return Send
 <|>  do
      _ <- reserved "recv"
      return Recv
 <|>  do
      _ <- reserved "deleg"
      return Delegate
 <|>  do
      _ <- reserved "resume"
      return $ Resume undefined
 <|>  do
      _ <- reserved "fst"
      return Fst
 <|>  do
      _ <- reserved "snd"
      return Snd
 <?> "Expecting a value"

pExpr :: P.Parser Exp
pExpr = do  v <- pValue
            return (V v)
    <|> do  _ <- reservedOp "req-"
            c <- identifier
            return (Req c (error "asd"))
    <|> do  _ <- reservedOp "acc-"
            c <- identifier
            return (Acc c (error "asd"))
    <|> do  c <- pConstant
            return (C c)
    <|> do  _   <- reserved "let" 
            x   <- identifier
            _   <- reservedOp "="
            e1  <- pTerm
            _   <- reserved "in"
            e2  <- pTerm
            return $  Let x e1 (abstract 0 x e2)  
    <|> do  _   <- reserved "if"  
            e1  <- pTerm
            _   <- reservedOp "then"
            e2  <- pTerm
            _   <- reserved "else"
            e3  <- pTerm
            return $ If e1 e2 e3
    <|> do  _ <- reservedOp "sel-"  
            l <- identifier
            e <- pStmt
            return (Select e l)
    <|> do  _ <- reserved "case"  
            e <- pTerm
--            _ <- reserved "with"
            list <- braces (P.sepBy (do 
                                      l <- identifier
                                      _ <- reservedOp ":"
                                      e' <- pTerm
                                      return (l, e'))
                                    (do
                                      _<- whiteSpace
                                      c<- P.char '|'
                                      _<- whiteSpace
                                      return c
                                    ))
            return (Match e list)
    <|> do  _ <- reserved "spawn"  
            e <- pStmt
            return (Spawn e)
     <|> parens (do 
            e1 <- pTerm
            -- parse either (e, e), (e e), (e)
            P.choice [ 
              P.try (        
                      do  _ <- reserved ","
                          e2 <- pTerm
                          case e1 of
                           V v1 -> case e2 of
                            V v2  -> return (V (PairV v1 v2) )
                            _     -> return (Pair e1 e2)
                           _    -> return (Pair e1 e2)
                      ),
              P.try (  do
--                        _ <- whiteSpace
                        e2 <- pTerm
                        return (App e1 e2))
                      ,
                      return e1])
     <|> do s <- identifier
            return (Free s)
     <?> "failed to match expression"


pStmt :: ParsecT String () Identity Exp
pStmt = do
  e1 <- pExpr
  P.choice [ P.try ( do e2 <- pStmt
                        return $ App e1 e2),
             return e1]
              

pTerm:: ParsecT String () Identity Exp
pTerm= do
    e1 <- pStmt
    P.choice [
              P.try (do _ <- reservedOp ";"
                        e2 <- pTerm
                        return $ Let "#" e1 e2),
              return e1] 
  


pTop :: P.Parser Exp
pTop = do
--        whiteSpace
        t <- pTerm
        P.eof
        return t





--lookupRLab :: Name -> StateT ( Map.Map String Unique ) IO Unique
--lookupRLab c = do
--  m <- get
--  case Map.lookup c m of
--    Just l -> return l 
--    Nothing        -> do
--      newSessionID <- lift newUnique
--      let m' = Map.insert c newSessionID m
--      put m'
--      return newSessionID

-- creates fresh annotations for request, accept, delegate and public channels
initAnnotations :: Exp -> IO Exp
initAnnotations e = case e of 
  Req c _ -> do 
            l <- newUnique
            return (Req c (R l))
  
  Acc c _ -> do 
            l <- newUnique
            return (Acc c (R l))
  
  -- all these cases are trivial
  V v  -> do v' <- initAnnotationsVal v
             return (V v')
  C c  ->  do c' <- initAnnotationsCon c
              return (C c')
  --
  Free _      -> return e
  Bound _ _   -> return e
  --
  Pair e1 e2  -> do e1' <- initAnnotations e1
                    e2' <- initAnnotations e2
                    return (Pair e1' e2')
  App e1 e2   -> do e1' <- initAnnotations e1
                    e2' <- initAnnotations e2
                    return (App e1' e2')
  Let x e1 e2 -> do e1' <- initAnnotations e1
                    e2' <- initAnnotations e2
                    return (Let x e1' e2')
  If e1 e2 e3 -> do e1' <- initAnnotations e1
                    e2' <- initAnnotations e2
                    e3' <- initAnnotations e3
                    return (If e1' e2' e3')
  --
  Select e1 l -> do e1' <- initAnnotations e1
                    return (Select e1' l)
  Match e1 es -> do e1' <- initAnnotations e1
                    es' <- mapM (\(l, e_i) -> do e_i' <- initAnnotations e_i
                                                 return (l, e_i')) es
                    return (Match e1' es')
  Spawn e1    -> do e1' <- initAnnotations e1
                    return (Spawn e1')
    

initAnnotationsVal :: Val -> IO Val
initAnnotationsVal v = case v of
--  PubChan c _ -> do 
--                    m <- get
--                    case Map.lookup c m of
--                      Just sessionID -> return (PubChan c (R sessionID))
--                      Nothing        -> do
--                        newSessionID <- lift newUnique
--                        let m' = Map.insert c newSessionID m
--                        put m'
--                        return (PubChan c (R newSessionID)) 
  Ses _ -> undefined
--                  do sesId     <- newUnique
--                    regionId  <- newUnique
--                    return (OpenSes p (newId)
  -- the other case are trivial
  U     -> return U
  TT    -> return TT
  FF    -> return FF
  N _   -> return v
  PairV v1 v2 -> do v1' <- initAnnotationsVal v1
                    v2' <- initAnnotationsVal v2
                    return (PairV v1' v2')
  Fun x e     -> do e' <- initAnnotations e
                    return (Fun x e')
  Fix f x e   -> do e' <- initAnnotations e
                    return (Fix f x e')

initAnnotationsCon :: Constant -> IO Constant 
initAnnotationsCon c = case c of
--  Request _ -> do newId <- liftIO newUnique
--                  return (Request (R newId))
--  Accept _  -> do newId <- liftIO newUnique
--                  return (Accept  (R newId))
  Resume _  -> do l <- newUnique
                  return (Resume  (R l))
--  --
  Send     -> return Send
  Recv     -> return Recv 
  Delegate -> return Delegate
  Fst      -> return Fst 
  Snd      -> return Snd
  
  
-- Either parses an expression, or exists with a parse error. 
-- The first (optional) arg is a filepath, the second arg is the input string.
parser :: String -> String -> IO Exp -- Either P.ParseError Exp
parser s filepath = case parse pTop s filepath of
  Left err -> error (show err)
  Right e  -> initAnnotations e


