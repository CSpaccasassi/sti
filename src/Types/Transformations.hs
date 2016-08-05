
module Types.Transformations where

import Types.TAE
import qualified Data.Set as Set
import Data.Unique

--import Debug.Trace (trace)

removeNils' :: Behaviour -> Behaviour
removeNils' (BVar i) = BVar i
removeNils' BNil             = BNil
removeNils' (BSeq BNil b2)   = removeNils' b2
removeNils' (BSeq b1 BNil)   = removeNils' b1
removeNils' (BSeq b1 b2)     = let b1' = removeNils' b1
                                   b2' = removeNils' b2
                                   b'  = BSeq  b1' b2'
                                in if b1' == BNil || b2' == BNil
                                  then removeNils' b'
                                  else b'
removeNils' (BPlus b1 b2)    = BPlus ( removeNils' b1 ) ( removeNils' b2 )
removeNils' (BPar b1 b2)     = BPar  ( removeNils' b1 ) ( removeNils' b2 )
removeNils' (BTrans k b1 b2) = BTrans k ( removeNils' b1 ) ( removeNils' b2 )
removeNils' b   = b

removeNils :: Constraint -> Constraint
removeNils ( BC b1 b2 ) = BC ( removeNils' b1 ) ( removeNils' b2 )
removeNils c = c

replaceT :: Unique -> Behaviour -> Type -> Type
replaceT _    _  (TVar i) = TVar i
replaceT _ _ TUnit    = TUnit
replaceT _ _ TBool    = TBool
replaceT _ _ TInt     = TInt
replaceT i b0 (TTup t1 t2)      = TTup ( replaceT i b0 t1 ) ( replaceT i b0 t2 )
replaceT i b0 (TArrowB t1 b t2) = TArrowB ( replaceT i b0 t1 ) ( replace i b0 b ) ( replaceT i b0 t2 )
replaceT i b0 (TChan t r)       = TChan (replaceT i b0 t) r



replace :: Unique -> Behaviour -> Behaviour -> Behaviour
replace i b0 (BVar i2)   = if i == i2 then b0 else BVar i2 
replace i b0 (BSeq b1 b2)     = BSeq     ( replace i b0 b1 ) ( replace i b0 b2)
replace i b0 (BPlus b1 b2)    = BPlus    ( replace i b0 b1 ) ( replace i b0 b2)
replace i b0 (BPar b1 b2)     = BPar     ( replace i b0 b1 ) ( replace i b0 b2)
replace i b0 (BNewChan t r)   = BNewChan ( replaceT i b0 t) r
replace i b0 (BSend r t)      = BSend    r  ( replaceT i b0 t)
replace i b0 (BRecv r t)      = BRecv    r  ( replaceT i b0 t)
replace i b0 (BTrans k b1 b2) = BTrans k (replace i b0 b1 ) ( replace i b0 b2 )
replace _ _  z = z


unfold' :: [Constraint] -> Constraints -> Behaviour -> Behaviour
unfold' [] _ b = b
unfold' (BC b0 (BVar beta0) : cs) cs' b = 
  if  length (filter (\c -> case c of BC _ (BVar beta') -> beta0 == beta'; _ -> False) ( Set.toList cs' ) ) > 1
--      || beta0 `Set.member` bvars (freevars b0)  
      || beta0 `Set.member` bvars (chanvars cs')
    then unfold' cs cs' b
    else unfold' cs cs' (replace beta0 b0 b) 
unfold' (_:cs) cs' b = unfold' cs cs' b

unfold :: Constraints -> Behaviour -> Behaviour
unfold cs = unfold' ( Set.toList cs ) cs