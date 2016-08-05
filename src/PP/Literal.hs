module PP.Literal where

import Text.PrettyPrint.HughesPJ ( text, (<>),  Doc, render, parens, hcat,
                                   braces,
                                   punctuate, integer, comma )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax
import Types
  
--------------------------------------------------------------------------------
-- Pretty Printing for Types - to be fed in Thea's session type checking tool
--------------------------------------------------------------------------------

-- constraints
ppConstraintsLit :: Constraints -> Doc
ppConstraintsLit cs = hcat (punctuate (text ",\n") 
                           $ map ppConstraintLit (Set.toList cs))
   

ppConstraintLit :: Constraint -> Doc
ppConstraintLit c = case c of
  TC t1 t2    -> ppTypeLit t1 <> text " < " <> ppTypeLit t2
  SC s sl     -> ppSesLabLit sl <> text " ~ " <> ppSesLit s
  BC b bvar   -> ppBLit b <> text " < " <> ppBetaLit bvar
  RC r rho    -> ppRegLit r <> text " ~ " <> ppRhoLit rho
  DC s1 s2    -> undefined -- not supported yet
  EVarC as is s -> let as' = (Map.toList as)
                       is' = (Map.toList is)
                   in ppSesLit (SExC as' is') 
                      <> text " ~ " 
                      <> text "S_ext_choice_" <> ppPsiLit s
  IVarC cs s   -> let cs' = (Map.toList cs)
                  in ppSesLit (SInC cs') 
                      <> text " ~ " 
                      <> text "S_ext_choice_" <> ppPsiLit s
              
-- typing context are not supported as of today
--ppTypingContext :: TypingContext -> Doc
--ppTypingContext ctx = hcat (punctuate (text ", ") (map ppTCItemLit ctx) )
--
--ppTCItemLit :: TCItem -> Doc
--ppTCItemLit (TSVar x ts) = text (x ++ ":") <> undefined --ppTS ts
--ppTCItemLit (TyVar x t ) = text (x ++ ":") <> undefined --ppType t

-- Type Schemas are not supported as of today
--ppTS :: TypeSchema -> Doc
--ppTS (TS gs cs t) = text "\\forall ("
--                  <> hcat gammas
--                  <> text " : "
--                  <> hcat constraints
--                  <> text "). "
--                  <> undefined --ppType t
-- where
--  gammas      = undefined --punctuate (comma <> text " ") (map ppGamma (Set.toList gs))
--  constraints = undefined --punctuate (comma <> text " ") (map ppConstraint (Set.toList cs))


ppAlphaLit :: Alpha -> Doc
ppAlphaLit (Alpha a) = text "T" <> ppUnique a

ppPsiLit :: Psi -> Doc
ppPsiLit (Psi p) = text "S" <> ppUnique p

ppRhoLit :: Rho -> Doc
ppRhoLit (Rho r) = text "R" <> ppUnique r

ppBetaLit :: Beta -> Doc
ppBetaLit (Beta bvar) = text "B" <> ppUnique bvar


  
-- type, session, region and behaviour variables
ppGammaLit :: Gamma -> Doc
ppGammaLit g = case g of
  TG t -> ppAlphaLit t
  SG s -> ppPsiLit s
  RG t -> ppRhoLit t
  BG b -> ppBetaLit b

-- Types
ppTypeLit :: Type -> Doc
ppTypeLit t = case t of
  TV    tvar -> ppAlphaLit tvar
  TUnit      -> text "unit"
  TBool      -> text "bool"
  TInt       -> text "int"
  --
  TPair    t1 t2    -> text "pair " <> ppTypeLit t1 
                       <> text " " <> ppTypeLit t2
  TBFun    t1 t2 b  -> text "funct " <> ppTypeLit t1 
                       <> text " -> " <> ppTypeLit t2
                       <> text " - " <> ppBetaLit b
  TSes  r   -> text " \\tses{ " <> ppReg r <> text "}" 

ppRegLit :: Reg -> Doc
ppRegLit (RV r) = ppRhoLit r
ppRegLit (RL r) = ppRLit r

ppRLit :: R -> Doc
ppRLit (R r) = text "$l" <> ppUnique r <> text "$" 

-- behaviours
ppBLit :: Behav -> Doc
ppBLit b = case b of
  BV bvar       -> ppBetaLit bvar 
  BTau          -> text "tau"
  BSeq    b1 b2 -> ppBLit b1 <> text ";\n" <> ppBLit b2
  BRec    b1 b2 -> text "rec " <> ppBetaLit b2 <> parens (ppBLit b1)
  BSpawn  b1    -> text "spn " <> parens (ppBLit b1)
  BIn     b1 b2 -> ppBLit b1 <> text " (+) "  <> ppBLit b2
  BPush   l  s  -> text "psh " <> parens (ppRLit l <> text ", " <> ppSesLit s)
  BOp    p      -> ppPopActLit p
  BEx  r cs     -> ppRegLit r <> text " ? optn {" <>  hcat (punctuate
                          (text ", " )
                          (map
                            (\(l_i, b_i) -> ppLabelLit l_i
                                            <> text " , "
                                            <> ppBLit b_i 
                            )
                            cs)) <> text "}"

-- behaviour pop actions
ppPopActLit :: BOps -> Doc
ppPopActLit pa = case pa of
  BSend   p  t  -> ppRegLit p  <> text " ! " <> ppTypeLit  t
  BRecv   p  t  -> ppRegLit p  <> text " ? " <> ppTypeLit  t
  BDeleg  p1 p2 -> ppRegLit p1 <> text " ! " <> ppRegLit   p2
  BResume p1 p2 -> ppRegLit p1 <> text " ? " <> ppRegLit   p2
  BInC    p l   -> ppRegLit p  <> text " ! " <> ppLabelLit l

ppLabelLit :: Label -> Doc
ppLabelLit l = text ("$" ++ l ++ "$")
              
-- Sessions
ppSesLit :: Ses -> Doc
ppSesLit s = case s of
  SV     psi   -> ppPsiLit psi
  EVar (Psi i) -> undefined  -- should not happen
  IVar (Psi i) -> undefined  -- should not happen
  --
  SEnd         -> text "end"
  --
  SSend  t s1     -> text " ! " <> ppTypeLit t <> text " " <> ppSesLit s1
  SRecv  t s1     -> text " ? " <> ppTypeLit t <> text " " <> ppSesLit s1
  --
  SDeleg   s1 s2  -> text " ! " <> ppSesLit s1 <> text " " <> ppSesLit s2
  SResume  s1 s2  -> text " ? " <> ppSesLit s1 <> text " " <> ppSesLit s2
  --
  SInC     ss     -> text " (+) " <> braces (hcat (punctuate comma
                          (map (\(l_i, s_i) -> text ("$l" ++ l_i ++ "$; ")
                                            <> ppSesLit s_i) ss)))
  SExC    as is   -> text " + "
                  -- active labels 
                  <> braces (hcat (punctuate comma
                          (map (\(l_i, s_i) -> text ("$l" ++ l_i ++ "$; ")
                                            <> ppSesLit s_i) as)))
                                <> text " "
                  -- inactive labels
                  <> braces (hcat (punctuate comma
                          (map (\(l_i, s_i) -> text ("$l" ++ l_i ++ "$; ")
                                            <> ppSesLit s_i) is)))       


-- session types
ppSesLabLit :: SesLab -> Doc
ppSesLabLit (SReq c (R l)) = text ('C':c) <> ppUnique l
ppSesLabLit (SAcc c (R l)) = text ('C':c) <> ppUnique l <> text "'"  

