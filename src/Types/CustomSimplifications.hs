
module Types.CustomSimplifications where

import Data.Maybe ( isJust )
import qualified Data.Set as Set
import Data.Unique

--import Debug.Trace ( trace )

import Types.TAE



cSubConstraint :: Constraint -> Constraint -> Constraint
cSubConstraint c (TC t1 t2)  = TC  ( cSubType c t1 )      ( cSubType c t2 )
cSubConstraint c (BC b1 b2)  = BC  ( cSubBehaviour c b1 ) ( cSubBehaviour c b2 )
cSubConstraint c (RC r1 r2)  = RC  ( cSubRegion c r1 )    ( cSubRegion c r2 )
cSubConstraint c (NTC t1 t2) = NTC ( cSubType c t1 )      ( cSubType c t2 )
--cSubConstraint c (NTC2 t1 t2) = NTC2 ( cSubType c t1 )      ( cSubType c t2 )


cSubConstraints :: Constraint -> Constraints -> Constraints
cSubConstraints c = Set.map (cSubConstraint c)

cSubType :: Constraint -> Type -> Type
cSubType (TC t (TVar j)) (TVar i)=
  if i == j
    then t
    else TVar i 
cSubType _ (TVar i) = TVar i
cSubType _ TUnit = TUnit
cSubType _ TBool = TBool 
cSubType _ TInt  = TInt
cSubType c (TTup t1 t2)      = TTup    (cSubType c t1) (cSubType c t2)
cSubType c (TArrowB t1 b t2) = TArrowB (cSubType c t1) (cSubBehaviour c b) (cSubType c t2)
cSubType c (TChan t r)       = TChan   (cSubType c t)  (cSubRegion c r) 


cSubBehaviour :: Constraint -> Behaviour -> Behaviour
cSubBehaviour (BC b (BVar j)) (BVar i) = 
  if i == j
    then b --trace (show i ++ " =?= " ++ show j ++ " --> " ++ show b) b
    else BVar i -- trace (show i ++ " =?= " ++ show j ++ " --> " ++ show (BVar i)) (BVar i)
cSubBehaviour _ (BVar i) = BVar i
cSubBehaviour _ BNil              = BNil
cSubBehaviour c (BSeq b1 b2)      = BSeq     (cSubBehaviour c b1) (cSubBehaviour c b2)
cSubBehaviour c (BPlus b1 b2)     = BPlus    (cSubBehaviour c b1) (cSubBehaviour c b2)
cSubBehaviour c (BPar b1 b2)      = BPar     (cSubBehaviour c b1) (cSubBehaviour c b2)
cSubBehaviour c (BNewChan t r)    = BNewChan (cSubType c t)       (cSubRegion c r)
cSubBehaviour c (BSend r t)       = BSend    (cSubRegion c r)     (cSubType c t)  
cSubBehaviour c (BRecv r t)       = BRecv    (cSubRegion c r)     (cSubType c t)
cSubBehaviour _ BTau              = BTau 
cSubBehaviour _ (BCommit k)       = BCommit k
cSubBehaviour c (BTrans k b1 b2)  = BTrans k (cSubBehaviour c b1) (cSubBehaviour c b2) 
 

cSubRegion :: Constraint -> Region -> Region
--cSubRegion _ (RLabel i) = RLabel i
cSubRegion (RC (RVar w) (RVar j)) (RVar i) = 
  if i == j
    then RVar w
    else RVar i
--cSubRegion _ (RVar i) = RVar i
cSubRegion _ r = r

--------------------------------------------------------------------------------
-- Garbage collection
--------------------------------------------------------------------------------
garbageCollect' :: Unique -> FreeVars -> Constraints -> Constraint -> Behaviour ->  
                                                Maybe ( Constraints, Behaviour )
garbageCollect' i vars cs c b = if i `fvMember` vars
                            then Nothing
                            else Just ( Set.delete c cs, b)


garbageCollect :: Constraints -> Behaviour -> Constraint -> Maybe ( Constraints, Behaviour )
garbageCollect cs b c@( TC t ( TVar i ) ) = garbageCollect' i ( freevars cs `unionFV` freevars b `unionFV` freevars t ) cs c b
garbageCollect cs b c@( BC b' ( BVar i )) = garbageCollect' i ( freevars cs `unionFV` freevars b `unionFV` freevars b') cs c b
garbageCollect cs b c@( RC r ( RVar i ) ) = garbageCollect' i ( freevars cs `unionFV` freevars b `unionFV` freevars r ) cs c b
garbageCollect _ _ _ = Nothing 


--------------------------------------------------------------------------------
-- Collapse links
--------------------------------------------------------------------------------
hasGammaLeft :: Unique -> Constraint -> Bool
hasGammaLeft i ( TC ( TVar j ) (TVar _)  ) = i == j
hasGammaLeft i ( BC ( BVar j ) (BVar _) ) = i == j
hasGammaLeft i ( RC ( RVar j ) (RVar _) ) = i == j
hasGammaLeft _ _ = False

combineConstraints :: Constraint -> Constraint -> Constraint
combineConstraints ( TC t1 ( TVar _ ) ) ( TC ( TVar _ ) t2 ) = TC t1 t2
combineConstraints ( BC b1 ( BVar _ ) ) ( BC ( BVar _ ) b2 ) = BC b1 b2
combineConstraints ( RC r1 ( RVar _ ) ) ( RC ( RVar _ ) r2 ) = RC r1 r2   
combineConstraints _ _ = error "unexpected combination of constraints"

collapseLinks' :: Constraints -> Behaviour -> Constraint ->  Unique 
                                            -> Maybe ( Constraints, Behaviour )
collapseLinks' cs b c i = let cs' = Set.filter ( hasGammaLeft i ) (Set.delete c cs)
                             in if i `fvMember` freevars b 
                                   || Set.size cs' /= 1
                                   || i `fvMember` freevars cs' 
                                  then Nothing
                                  else let c''   = head ( Set.toList cs' )
                                           c'''  = combineConstraints c c''
                                           cs''  = Set.delete c ( Set.delete c'' cs )
                                           cs''' = Set.insert c''' cs''
                                       in Just (cs''' , b)

collapseLinks :: Constraints -> Behaviour -> Constraint ->  Maybe ( Constraints, Behaviour )
collapseLinks cs b c@( TC _ ( TVar i ) )  = collapseLinks' cs b c i
collapseLinks cs b c@( BC _ ( BVar i ) )  = collapseLinks' cs b c i
collapseLinks cs b c@( RC _ ( RVar i ) )  = collapseLinks' cs b c i
collapseLinks _ _ _                       = Nothing


--------------------------------------------------------------------------------
-- Substitute leaves
--------------------------------------------------------------------------------

rightFreevars  :: Constraint -> FreeVars
rightFreevars ( TC _ t ) = freevars t
rightFreevars ( BC _ b ) = freevars b
rightFreevars ( RC _ r ) = freevars r
rightFreevars ( NTC _ t ) = freevars t
--rightFreevars ( NTC2 _ t ) = freevars t


isLabelConstraint :: Constraint -> Bool
isLabelConstraint ( RC (RLabel _) _ )  = True
isLabelConstraint ( RC (RRLabel _) _ ) = True
isLabelConstraint _                    = False

findNonSubstitutableAlphas :: [Constraint] -> [Unique]
findNonSubstitutableAlphas ( NTC (TVar i) _ : cs) =  i : findNonSubstitutableAlphas cs   
--findNonSubstitutableAlphas ( NTC2 (TVar i) _ : cs) =  i : findNonSubstitutableAlphas cs
findNonSubstitutableAlphas ( _ : cs)              =  findNonSubstitutableAlphas cs
findNonSubstitutableAlphas []                     = []

substituteLeaves' :: Constraints -> Behaviour -> Constraint -> Unique -> 
                    FreeVars ->  Maybe ( Constraints, Behaviour )
substituteLeaves' cs b c i fvs = 
  if isLabelConstraint c 
     ||i `fvMember` fvs 
     || i `fvMember` Set.fold (\c' acc -> rightFreevars c' `unionFV` acc ) emptyFreeVars ( Set.delete c cs )
     || i `elem` findNonSubstitutableAlphas (Set.toList cs)
   then Nothing
   else Just ( cSubConstraints c (Set.delete c cs ), cSubBehaviour c b )

substituteLeaves :: Constraints -> Behaviour -> Constraint ->  Maybe ( Constraints, Behaviour )
substituteLeaves cs b c@( TC t ( TVar i ) ) = substituteLeaves' cs b c i ( freevars t ) 
substituteLeaves cs b c@( BC b'( BVar i ) ) = substituteLeaves' cs b c i ( freevars b')
substituteLeaves cs b c@( RC r ( RVar i ) ) = substituteLeaves' cs b c i ( freevars r )
substituteLeaves _  _ _                     = Nothing

--------------------------------------------------------------------------------
-- Constraints simplification
--------------------------------------------------------------------------------

isGammaRight :: Constraint -> Bool
isGammaRight ( TC _ ( TVar _ ) ) = True
isGammaRight ( BC _ ( BVar _ ) ) = True
isGammaRight ( RC _ ( RVar _ ) ) = True
isGammaRight _ = False

simplifyConstraints :: Constraints -> Behaviour -> (Constraints, Behaviour)
simplifyConstraints cs b = 
  let candidates = Set.filter isGammaRight cs
  in if Set.null candidates
      then (cs, b)
      else let  c = foldr ( \f acc ->
                      if isJust acc  
                       then acc 
                       else Set.fold ( \x acc' ->
                                    if isJust acc'
                                     then acc'
                                     else f cs b x ) 
                                  Nothing 
                                  candidates 
                         ) 
                          Nothing
                          [garbageCollect, substituteLeaves, collapseLinks] 
  in case c of
    Nothing -> (cs, b)
    Just (cs', b')  -> simplifyConstraints cs' b'