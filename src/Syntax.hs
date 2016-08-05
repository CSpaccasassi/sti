module Syntax where

import Control.Arrow (second)
import Data.Unique (Unique, hashUnique, newUnique)
import Text.PrettyPrint.HughesPJ( text, (<>), Doc, render, parens, --nest,($$), 
                                  -- braces, 
                                  hcat, punctuate, comma, integer  )

--import PrettyPrinting


-- Simple data
type Label  = String
type Name   = String
type SessID = Unique

{- Instead of being included in their type definitions, variables (for sessions 
  and regions) are defined as separate data structures.
  The reason is that they do not only occur in types, but they also annotate 
  syntax and other types, e.g. type schemas and the request and accept keywords.  
  Therefore it is more convenient to treat them as entities on their own.  -} 
data Alpha = Alpha  Unique     deriving (Eq, Ord)  -- Type variables
data Psi   = Psi    Unique     deriving (Eq, Ord)  -- Session variables
data Beta  = Beta   Unique     deriving (Eq, Ord)  -- Behaviour Variables
data Rho   = Rho    Unique     deriving (Eq, Ord)  -- Region Variables

-------------------------------------------
-- Regions
-------------------------------------------
data Reg = RV Rho     -- region variable
         | RL R       -- region label    
                              deriving ( Eq, Ord )

data R = R Unique         deriving (Eq, Ord)  -- Region label

-------------------------------------------
-- Expressions
-------------------------------------------
data SLabel = SL Unique deriving (Eq, Ord)

-- Values
data Val = 
    U                           -- unit
  | TT                          -- true
  | FF                          -- false
  | N       Integer             -- integer
  | PairV   Val     Val         -- tuple
--  | PubChan Name    R           -- public channel, with name and region label
  | Ses     Reg                 -- open session, with unique ID and region k
  | Fun     Name    Exp         -- function, with argument name and 
  | Fix     Name    Name    Exp -- fix-point
    deriving Eq

-- Expressions
data Exp =
    -- values 
      V Val
    -- variables
    | Free  Name
    | Bound Name Int              -- Int is a De-Brujin index
    -- 
    | Pair Exp   Exp
    | App  Exp   Exp
    | Let  Name  Exp Exp
    | If   Exp   Exp Exp
    -- concurrency
    | C       Constant
    | Req     Name R
    | Acc     Name R
    | Select  Exp Label
    | Match   Exp [(Label, Exp)]
    | Spawn   Exp                     deriving (Eq)


-- Constants
data Constant =   Send
                | Recv
                | Delegate
                | Resume R         
                | Fst
                | Snd deriving Eq




----------------------
-- De-Brujin indexes
----------------------
------- creates De-Brujin indexes -------
abstract :: Int -> String -> Exp -> Exp
abstract i s e = case e of 
  V v           ->  V (abstractVal i s v)
  Free s'       -> if s == s' then Bound s' i 
                              else Free  s'
  Bound _ _     -> e
  If e1 e2 e3     -> If (abstract i s e1) (abstract i s e2) (abstract i s e3)
  Let x t1 t2     -> if x == s 
                      then Let x (abstract i s t1)    t2 
                      else Let x (abstract i s t1)    (abstract (i+1) s t2)
                    --Let x (abstract i s t1)    (abstract (i+1) s t2)
  App t1 t2       -> App       (abstract i s t1)    (abstract i s t2)
  
  Pair t1 t2       -> Pair (abstract i s t1)    (abstract i s t2)

  C _           -> e
  Select t l    -> Select (abstract i s t) l 
  Match t tlist -> let t'     = abstract i s t 
                       tlist' = map (Control.Arrow.second (abstract i s)) tlist 
                   in Match t' tlist'
  Spawn t       -> Spawn (abstract i s t)
  Req c l       -> Req c l
  Acc c l       -> Acc c l



abstractVal :: Int -> String -> Val -> Val
abstractVal i s v = case v of 
  U     -> U
  TT    -> TT
  FF    -> FF
  N _   -> v
  PairV e1 e2 -> PairV (abstractVal i s e1) (abstractVal i s e2)
  Fun x e     -> if s == x
                  then v
                  else Fun x (abstract (i+1) s e)
  Fix f x e   -> if s == f || s == x
                  then v
                  else Fix f x (abstract (i+2) s e)
--  PubChan _ _ -> v
  Ses _       -> v



----------------------
-- Pretty Printing
----------------------

-- prints uniques
ppUnique :: Unique -> Doc
ppUnique i = text ( show (hashUnique i) )

-- prints annotations
instance Show Alpha where show = render . ppAlpha
ppAlpha :: Alpha -> Doc
ppAlpha (Alpha i) = text "\\alpha_{" <> ppUnique i <> text "}"

instance Show Beta where show = render . ppBeta
ppBeta :: Beta -> Doc
ppBeta (Beta i) = text "\\beta_{" <> ppUnique i <> text "}"

instance Show Psi where show = render . ppPsi
ppPsi :: Psi -> Doc
ppPsi (Psi i)    = text "\\psi_{" <> ppUnique i <> text "}"
--ppPsi (VarPsi i) = text "\\psi_{" <> ppUnique i <> text "}"
--ppPsi (BarPsi i) = text "\\overline{\\psi_{" <> ppUnique i <> text "}}"


instance Show Reg where show = render . ppReg
ppReg :: Reg -> Doc
ppReg (RV r)  = ppRho r
ppReg (RL k)  = ppRL k

instance Show Rho where show = render . ppRho
ppRho :: Rho -> Doc
ppRho (Rho i) = text "\\rho_{" <> ppUnique i <> text "}"


instance Show R where show = render . ppRL
ppRL :: R -> Doc
ppRL (R i) = text "l_{" <> ppUnique i <> text "}"




instance Show SLabel where show = render . ppSLabel

ppSLabel :: SLabel -> Doc
ppSLabel (SL i) = text ( "p_{" ++ show (hashUnique i) ++ "}" )


-- print values
instance Show Val where show = render . ppVal 0

-- print expressions
instance Show Exp where show = render . pp  

-- pretty prints an expression. 
pp :: Exp -> Doc
pp = ppExpr 0


ppExpr :: Int -> Exp -> Doc
ppExpr i e = case e of              -- i is the indentation level
  V v -> ppVal i v -- ppValue i v
  C c -> ppConst c 
  --
  Req c l -> text "\\erequest{ " <> ppRL l <> text (" }{" ++ c ++ "}") 
  Acc c l -> text "\\eaccept{ "  <> ppRL l <> text (" }{" ++ c ++ "}")
  --
  Free x    ->  text x
  Bound x _ ->  text x
  --
  App e1 e2   -> parens ( ppExpr i e1 
                          <> text "~" 
                          <> ppExpr i e2 )
  Let x e1 e2 -> text ("\\elet{" ++ x ++"}{") 
                   <> ppExpr (i+1) e1 
                   <> text "}{" 
                   <> ppExpr i e2
                   <> text "}"
  If e1 e2 e3 -> text "\\eif{" <> ppExpr i e1 
                      <> text "}{" <> ppExpr i e2 
                      <> text "}{" <> ppExpr i e3
                      <> text "}"

  Pair e1 e2  -> parens (ppExpr i e1 <> text ", " <> ppExpr i e2)
  Select e1 l -> text ("\\eselectnew{" ++ l ++ " }~") <> ppExpr i e1
  Match e1 es -> text "\\ematch{" <> ppExpr i e1 <> text "}{" <> ppMList i es 
                  <> text "}{}"
  Spawn e1    -> text "\\espawn " <> parens (ppExpr i e1)


ppMList :: Int -> [(Label, Exp)] -> Doc
ppMList i ls = 
  let ls' = punctuate (comma <> text " ") (map (f i) ls)
  in   (hcat ls')
    where f j (l, e) = text ( l ++ " : ") <> ppExpr j e 


ppVal :: Int -> Val -> Doc
ppVal i v = case v of
  U   -> text "()"
  TT  -> text "\\ctrue"
  FF  -> text "\\cfalse"
  N n -> integer n
  --
  PairV v1 v2 -> parens (ppVal i v1 <> text ", " <> ppVal i v2 ) 
  Fun x e     -> text ("\\efunc{" ++ x ++ "}{") <> ppExpr i e <> text "}"
  Fix f x e   -> text ("\\efix{" ++ f ++ "}{" ++ x ++ "}{")
                 <> ppExpr i e <> text "}"
  --
--  PubChan c chId  -> text ("\\echan{" ++ c ++ "}{" ++ show chId ++ "}") 
  Ses     p       -> text ("\\schan{" ++ show p ++ "}{}")


ppConst :: Constant -> Doc
ppConst c = case c of
--  Request -> text ("\\erequest{}{}") 
--  Accept  -> text ("\\eaccept{}{}")
  Resume l -> text "\\eresume{" <> ppRL l <> text "}{}"
  --
  Send -> text "\\esend{}"
  Recv -> text "\\erecv{}{}"
  --
  Delegate -> text "\\edeleg{}"
  Fst      -> text "\\mathtt{fst}"
  Snd      -> text "\\mathtt{snd}"
  
  
  
----------------------
-- Utilities
----------------------

newAlpha :: IO Alpha
newAlpha = do
  freshId <- newUnique
  return (Alpha freshId)

newPsi :: IO Psi
newPsi = do
  freshId <- newUnique
  return (Psi freshId)

newSLabel :: IO SLabel
newSLabel = do
  freshId <- newUnique
  return (SL freshId)


newBeta :: IO Beta
newBeta = do
  freshId <- newUnique
  return (Beta freshId)

newRho :: IO Rho
newRho = do
  freshId <- newUnique
  return (Rho freshId)
