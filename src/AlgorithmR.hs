--
module AlgorithmR where


import Syntax
import Types

--import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set  
import Data.Unique
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
--import Data.Graph.Inductive.Graphviz

import Data.Graph.Inductive.Query.DFS (reachable)
import Data.Maybe ( mapMaybe, isJust )
--import qualified Data.Map as Map
--import Debug.Trace (trace)

type Configuration = (Constraints, Type, Behav)


makeSubstToRight :: Constraint -> Substitution
makeSubstToRight c = case c of 
  TC (TV (Alpha i1)) (TV (Alpha i2)) -> mkTSub i2 i1   
  --
--  SC (SV (Psi i1))    (Psi i2)    -> mkSSub i2 i1
  --
  BC (BV (Beta i1)) (Beta i2) -> mkBSub i2 i1
  --
  RC (RV (Rho i1))  (Rho i2)  -> mkRSub i2 i1
  --
  _                      -> idSub  

makeSubstToLeft :: Constraint -> Substitution
makeSubstToLeft c = case c of 
  TC (TV (Alpha i1)) (TV (Alpha i2))  -> mkTSub i1 i2   
  --
--  SC (SV (Psi i1))    (Psi i2) -> mkSSub i1 i2
--  SC (Psi i1) (BarPsi i2) -> mkSSub i1 i2
  --
  BC (BV (Beta i1)) (Beta i2) -> mkBSub i1 i2
  --
  RC (RV (Rho i1)) (Rho i2) -> mkRSub i1 i2
  --
  _                      -> idSub



--------------------------------------------------------------------------------
-- Graph manipulation
--------------------------------------------------------------------------------
varToNode :: Unique -> LNode Unique
varToNode i = (hashUnique i, i) 


varsToEdge :: Unique -> Unique -> Edge
varsToEdge i1 i2 = (hashUnique i1, hashUnique i2)


-- edges go from variable i2 to i1, to reflect the fact that ( TV i1 ) <= ( TV i2 )
constraintToEdge :: Constraint -> LEdge ()
constraintToEdge ( TC (TV (Alpha i1)) ( TV (Alpha i2) ) ) = ( hashUnique i2, hashUnique i1, () )
--constraintToEdge ( SC (SV (Psi i1))   ( Psi i2 ) ) = ( hashUnique i2, hashUnique i1, () )
constraintToEdge ( BC (BV (Beta i1))  ( Beta i2 ) ) = ( hashUnique i2, hashUnique i1, () )
constraintToEdge ( RC (RV (Rho i1))   ( Rho i2 ) ) = ( hashUnique i2, hashUnique i1, () )

constraintToEdge _ = error "expecting atomic constraint"


rmEdge :: Unique -> Unique -> Gr Unique () -> Gr Unique ()
rmEdge i1 i2 = delEdge (varsToEdge i1 i2)

 
--------------------------------------------------------------------------------
-- Transformation: redundant
--------------------------------------------------------------------------------
isRedundants' :: Unique -> Unique -> Gr Unique () -> Bool
isRedundants' i1 i2 g =
  -- check if there is an alternative route from i2 to i1, other than the direct
  -- one
  let n1 = hashUnique i2
      n2 = hashUnique i1 
      e  = ( n2, n1 )
      g' = delEdge e g
  in ( not . null ) (esp n2 n1 g') || ( n1 == n2 )     


isRedundant :: Gr Unique () -> Constraint -> Bool
isRedundant g (TC (TV (Alpha i1)) (TV (Alpha i2)) ) = isRedundants' i1 i2 g 
isRedundant g (BC (BV (Beta i1))  (Beta i2) ) = isRedundants' i1 i2 g
--isRedundant g (SC (SV (Psi i1))   (Psi i2) ) = isRedundants' i1 i2 g --TODO: reinsert?
isRedundant g (RC (RV (Rho i1))   (Rho i2) ) = isRedundants' i1 i2 g
isRedundant _                         _ = False


removeRedundant :: TypingContext -> Configuration -> Maybe Configuration
removeRedundant _ ( cs, t, b ) =
  let g  = mkAtomicGraph cs
      rs = Set.filter ( isRedundant g ) cs
  in case Set.toList rs of
    []  -> Nothing
    c:_ -> Just ( Set.delete c cs, t, b )    


--------------------------------------------------------------------------------
-- Transformation: remove cycles
--------------------------------------------------------------------------------


hasCycle'' :: TypingContext -> Gr Unique () -> Unique -> Unique -> Bool
hasCycle'' tc g i1 i2 =
  let n1 = hashUnique i2
      n2 = hashUnique i1 
  in  i1 /= i2  
      && not ( i1 `fvMember` freevars tc )
      && (n1 `elem` reachable n2 g) 
      && (n2 `elem` reachable n1 g) -- has cycle
--      && not (inNewType i1 cs)
--      && not (inNewType i2 cs)

--      then if not (i1 `fvMember` chanvars cs)
--            then 
--            else if not (i2 `fvMember` chanvars cs )
--                  then Just ( makeSubstToLeft c )
--                  else Nothing 
--      else Nothing

hasCycle' :: TypingContext -> Gr Unique () -> Constraint -> Bool
hasCycle' tc g ( TC (TV (Alpha i1)) (TV (Alpha i2)) ) = hasCycle'' tc g i1 i2
--hasCycle' tc g ( SC (SV (Psi i1))   (Psi i2) )        = hasCycle'' tc g i1 i2
hasCycle' tc g ( BC (BV (Beta i1))  (Beta i2) )       = hasCycle'' tc g i1 i2
hasCycle' tc g ( RC (RV (Rho i1))   (Rho i2) )        = hasCycle'' tc g i1 i2
hasCycle' _ _ _                                       = False


-- TODO: is this necessary with session types?
--makeCVFreeSubst' :: Constraints -> Constraint -> Unique -> Unique -> 
--                      Maybe Substitution
--makeCVFreeSubst' cs c i1 i2
--  | not (i1 `fvMember` chanvars cs) = Just (makeSubstToRight c)
--  | not (i2 `fvMember` chanvars cs) = Just (makeSubstToLeft c)
--  | otherwise                     = Nothing


--makeCVFreeSubst :: Constraints -> Constraint -> Substitution
--makeCVFreeSubst cs c@( TC (TV (Alpha i1)) (TV (Alpha i2)) )     = makeSubstToLeft cs c i1 i2
--makeCVFreeSubst cs c@( SC (Psi i1) (Psi i2) )   = makeSubstToLeft cs c i1 i2
--makeCVFreeSubst cs c@( BC (Beta i1) (Beta i2) ) = makeSubstToLeft cs c i1 i2
--makeCVFreeSubst cs c@( RC (Rho i1) (Rho i2) )   = makeSubstToLeft cs c i1 i2
--makeCVFreeSubst _  _                            = idSub



findNonSubstitutableAlphas :: [Constraint] -> [Unique]
--findNonSubstitutableAlphas ( NTC (TV i) _ : cs) =  i : findNonSubstitutableAlphas cs   
findNonSubstitutableAlphas ( _ : cs)              =  findNonSubstitutableAlphas cs
findNonSubstitutableAlphas []                     = []






maybeCycle :: TypingContext -> Gr Unique () -> Constraint -> Maybe Substitution
maybeCycle tc g c = if hasCycle' tc g c 
                      then 
--                      case makeCVFreeSubst cs c of
--                        Nothing -> Nothing
                         Just (makeSubstToLeft c)
--                          let tsubst'     = tsubst
--                              sigma'       = Sub tsubst' bsubst rsubst
----              in Just sigma
--                          in if Map.size tsubst /= Map.size tsubst'
--                              then Just ( trace "cycle" sigma')     
--                              else Just ( trace "cycle" sigma )
                      else Nothing


removeCycle :: TypingContext -> Configuration  -> Maybe Configuration
removeCycle  tc (cs, t, b ) =
  let g      = mkAtomicGraph cs
      cycles = mapMaybe ( maybeCycle tc g ) ( Set.toList cs )
  in case cycles of
    []        -> Nothing
    ( s : _)  -> Just ( apply s cs, apply s t, apply s b )   
                  
                  

--------------------------------------------------------------------------------
-- Transformation: shrink
--------------------------------------------------------------------------------
inRHS :: Unique -> Constraint -> Bool
inRHS i ( TC ( TV (Alpha _) ) ( TV (Alpha i2) ) ) = i == i2
--inRHS i ( SC ( SV (Psi _) )   ( Psi i2 ) ) = i == i2
inRHS i ( BC ( BV (Beta _) )  ( Beta i2 ) ) = i == i2
inRHS i ( RC ( RV (Rho _) )   ( Rho i2 ) ) = i == i2
inRHS _  _                            = False 


canShrink :: TypingContext -> Configuration -> Constraint -> Bool
canShrink tc (cs, t, b) c@( TC ( TV (Alpha i1) ) ( TV (Alpha i2) ) )     = canShrink' i1 i2 tc ( Set.delete c cs, t, b )
--canShrink tc (cs, t, b) c@( SC ( SV (Psi i1) )   ( Psi i2 ) )   = canShrink' i1 i2 tc ( Set.delete c cs, t, b )
canShrink tc (cs, t, b) c@( BC ( BV (Beta i1) )  ( Beta i2 ) )  = canShrink' i1 i2 tc ( Set.delete c cs, t, b )
canShrink tc (cs, t, b) c@( RC ( RV (Rho i1) )   ( Rho i2 ) )   = canShrink' i1 i2 tc ( Set.delete c cs, t, b )
canShrink _ _ _                                          = False


canShrink' :: Unique -> Unique -> TypingContext -> Configuration -> Bool
canShrink' g' g tc (cs, t, b) = 
  g' /= g  
  && not ( g `fvMember` freevars tc )
--  && not ( g `fvMember` freevars e )
--  && not ( g' `fvMember` freevars e ) --TODO: shouldn't this be included?
  && not (any (inRHS g) cs') 
  && g `isMonotonic` t
  && g `isMonotonic` b
  && all (isMonotonic g)  cs'
--  && not (inNewType g cs)
--  && not (inNewType g' cs)
    where cs' = Set.toList cs


shrink :: TypingContext -> Configuration  -> Maybe Configuration 
shrink tc conf@(cs, t, b) =
  let (shrinkable, _) = Set.partition ( canShrink tc conf ) cs
  in case Set.toList shrinkable of
    []        -> Nothing
    ( c : _)  -> Just --trace "Shrink" 
                  ( apply s cs, apply s t, apply s b )   
                  where s = makeSubstToRight c
  
    
--------------------------------------------------------------------------------
-- Transformation: boost
--------------------------------------------------------------------------------
canBoost :: TypingContext -> Configuration -> Constraint -> Bool
canBoost tc (cs, t, b) c@( TC (TV (Alpha i1)) (TV (Alpha i2)) ) = canBoost' i1 i2 tc ( Set.delete c cs, t ,b )   
--canBoost tc (cs, t, b) c@( SC (SV (Psi i1))   (Psi i2) ) = canBoost' i1 i2 tc ( Set.delete c cs, t, b )
canBoost tc (cs, t, b) c@( BC (BV (Beta i1))  (Beta i2) ) = canBoost' i1 i2 tc ( Set.delete c cs, t, b )
canBoost tc (cs, t, b) c@( RC (RV (Rho i1))   (Rho i2) ) = canBoost' i1 i2 tc ( Set.delete c cs, t, b )
canBoost _ _ _                                      = False 


canBoost' :: Unique -> Unique -> TypingContext ->Configuration -> Bool
canBoost' i1 i2 tc (cs, t, b) = 
  i1 /= i2  
  && not ( i1 `fvMember` freevars tc )
  && i1 `isAntiMonotonic` t
  && i1 `isAntiMonotonic` b
  && all (isAntiMonotonic i1) ( Set.toList cs )
--  && not (inNewType i1 cs)
--  && not (inNewType i2 cs)

boost :: TypingContext -> Configuration  -> Maybe Configuration
boost tc conf@(cs, t, b) = do
  let (boostable, _) = Set.partition (canBoost tc conf) cs
  case Set.toList boostable of
    []        -> Nothing
    ( c : _)  -> Just ( --trace "Boost" $ 
                        apply s cs, apply s t, apply s b )
                   where s = makeSubstToLeft c   


joinSession' :: [Constraint] -> Configuration -> Map.Map SesLab Psi -> Configuration
joinSession' [] conf _ = conf
joinSession' (SC (SV( Psi i)) l:cs) cfg@(cs', t, b) m = 
  case Map.lookup l m of
    Just (Psi j) -> 
      let s = mkSSub i j
      in joinSession' cs (apply s cs', apply s t, apply s b) m
    Nothing -> 
      let m' = Map.insert l (Psi i) m
      in joinSession' cs cfg m'

joinSession' (_:cs) conf m = joinSession' cs conf m

joinSession :: TypingContext -> Configuration -> Maybe Configuration
joinSession _ cfg@(cs, _, _) = 
  let cfg'@(cs', _, _) = joinSession' (Set.toList cs) cfg Map.empty
  in if Set.size cs == Set.size cs' 
      then Nothing 
      else Just cfg'  

--------------------------------------------------------------------------------
-- Algorithm R
--------------------------------------------------------------------------------
algorithmR :: TypingContext -> Configuration  -> Configuration
algorithmR tc conf =
  let c = foldr ( \f acc -> if isJust acc then acc else f tc conf ) Nothing 
            [removeRedundant, removeCycle, shrink, boost, joinSession] 
  in case c  of
    Nothing -> conf
    Just conf'  -> algorithmR tc --(trace ("conf' = " ++ show conf' ) 
                                 conf' --) 
    

isAtomic :: Constraint -> Bool
isAtomic ( TC (TV (Alpha _)) (TV (Alpha _)) ) = True
--isAtomic ( SC (SV (Psi _))   (Psi _) ) = True
isAtomic ( BC (BV (Beta _))  (Beta _) ) = True
isAtomic ( RC (RV (Rho _))   (Rho _) ) = True
isAtomic _                        = False  



mkAtomicGraph :: Constraints -> Gr Unique ()
mkAtomicGraph cs = let  acs                = Set.filter isAtomic cs
--                        acs'               = (Set.filter (\c-> case c of NTC _ _ -> False; NTC2 _ _ -> False; _ -> True) acs)
--                        acs'               = (Set.filter (\c-> case c of NTC _ _ -> False; _ -> True) acs)
                        ( FreeVars ts ss bs rs ) = freevars acs
                        vars                  = Set.toList ts ++ Set.toList ss ++ Set.toList bs ++ Set.toList rs
                        varNodes              = map varToNode vars
                        varEdges              = Set.toList $ Set.map constraintToEdge acs
                        g                     = mkGraph varNodes varEdges
                   in g


--simplify :: Constraints -> Type -> Behaviour -> TypingContext -> IO ( Constraints, Type, Behaviour )
--simplify cs t b tc = do
--  let g = mkAtomicGraph cs
--      (cs', t', b') = doAlgorithmR cs t b g
--  if Set.size cs == Set.size cs'
--    then return (cs', t', b')
--    else simplify cs' t' b'