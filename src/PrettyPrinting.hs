module PrettyPrinting where
--
--import Data.Unique (hashUnique)
--import Syntax



--
--
--ppOp :: String -> Expression -> Expression -> Doc
--ppOp op e1 e2 = parens $ ppExpr 0 e1 <> text (" " ++ op ++ " ") <> ppExpr 0 e2
--
--
--
--
--
--
--
--instance Show Value where show = render . ppValue 0
--
--instance Show Type where
--  show = render . ppType 0
--
---- pretty printing
--ppType :: Int -> Type -> Doc
--ppType _ (TVar t)           = text $ 'a' : show ( hashUnique t )
--ppType _ TInt               = text "Int"
--ppType _ TBool              = text "Bool"
--ppType _ TUnit              = text "Unit"
--ppType i (TTup t1 t2)       = parens ( ppType i t1 <> text ", " <> ppType i t2 )
--ppType i (TArrowB t1 b t2)  = parens ( ppType i  t1 ) <> text " -" <> 
--                              parens ( ppBehaviour i b ) <> text "-> " <> 
--                              parens ( ppType i t2 )  
--ppType i (TChan t r)        = parens ( ppType i t <> text " Chan " <> ppRegion r )
--
--
--
--
--instance Show Behaviour where
--  show = render . ppBehaviour 0  
--  
---- pretty printing
--ppBehaviour :: Int -> Behaviour -> Doc
--ppBehaviour _ (BVar j)  = text $ 'b' : show ( hashUnique j )
--ppBehaviour _ BNil             = text "nil"
--
--ppBehaviour i (BSeq b1@(BNewChan _ _) b2)     
--                               = ppBehaviour i b1 <> text ".(" $$ 
--                                 nest (i+1) ( ppBehaviour (i+1) b2 <> text " )")
--ppBehaviour i (BSeq b1 b2)     = ppBehaviour i b1 <> text ". " <> ppBehaviour i b2
--ppBehaviour i (BPlus b1 b2)    = parens (ppBehaviour i b1) <> text " + " <> parens (ppBehaviour i b2) 
--ppBehaviour i (BPar b1 b2)     = ppBehaviour i b1 $$ text "|| " <> ppBehaviour i b2
--ppBehaviour i (BNewChan t r)   = text "newchan " <> ppType i (TChan t r )   
--ppBehaviour i (BSend r t)      = ppRegion r <> text "!" <> ppType i t  
--ppBehaviour i (BRecv r t)      = ppRegion r <> text "?" <> ppType i t
--ppBehaviour _ BTau             = text "tau"
--
--
--
--
--instance Show Region where
--  show = render . ppRegion
--
---- pretty printing
--ppRegion :: Region -> Doc
--ppRegion ( RVar j )   = text $ 'r' : show ( hashUnique j )
--ppRegion ( RLabel j ) = text $ 'l' : show ( hashUnique j )
--ppRegion ( RRLabel j ) = text $ "lr_" ++ show ( hashUnique j )
--
--
--
--
--
--instance Show Constraint where
--  show (TC t1 t2) = "( " ++ show t1 ++ " ) <= ( " ++ show t2 ++ " )"
--  show (BC b1 b2) = "( " ++ show b1 ++ " ) <= ( " ++ show b2 ++ " )"
--  show (RC r1 r2) = "( " ++ show r1 ++ " ) <= ( " ++ show r2 ++ " )"
--  show (NTC t1 t2) = "newtype( " ++ show t1 ++ " ) <= ( " ++ show t2 ++ " )"
----  show (NTC2 t1 t2) = "newtype2( " ++ show t1 ++ " ) <= ( " ++ show t2 ++ " )"
