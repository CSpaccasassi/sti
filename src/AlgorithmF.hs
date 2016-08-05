
module AlgorithmF where

import Syntax
import Types

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
--import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.Query.DFS
import Data.Unique
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Control.Arrow (second)
import Data.Graph.Inductive.Basic (undir)

--import Debug.Trace (trace)

type TVarEquivalence = Gr Unique ()

breakConstraints :: [Constraint] -> Maybe [Constraint]
breakConstraints []   = Nothing
breakConstraints (c:cs) = case c of
  -- remove redundant constraints
  TC TUnit TUnit -> Just cs
  TC TBool TBool -> Just cs
  TC TInt  TInt  -> Just cs
  -- decompose pair and function types
  TC (TPair t1 t2)   (TPair t1' t2')    -> Just ([TC t1 t1', TC t2 t2'] ++ cs) 
--  TC (TFun t1 t2)    (TFun t1' t2')     -> Just ([TC t1' t1, TC t2 t2'] ++ cs)
  TC (TBFun t1 t2 b) (TBFun t1' t2' b') -> Just ([TC t1' t1, TC t2 t2', BC (BV b) b']  ++ cs) 
  -- recursive step
  TC (TSes (RL l1) )   (TSes (RL l2)) -> if l1 == l2 then Just cs
                                                     else Nothing
  TC (TSes (RV rho) ) (TSes r)        -> Just (RC r rho : cs)  
  TC (TSes r )        (TSes (RV rho)) -> Just (RC r rho : cs)
  _              -> case breakConstraints cs of
    Just cs' -> Just (c:cs')
    Nothing -> Nothing


--

--breakTChans :: [Constraint] -> Maybe (Substitution, [Constraint])
--breakTChans []   = Nothing
--breakTChans (c:cs) = case breakTChan c of
--  Just x  -> Just (x, map (apply x) cs)
--  Nothing -> case breakTChans cs of
--    Just (x, cs') -> Just (x, apply x c:cs')
--    Nothing       -> Nothing

--breakTChan :: Constraint -> Maybe Substitution
--breakTChan (TC (TChan (SL i)) (TChan (SL j)) ) = Just ( mkSLSub i j )
--breakTChan _                                   = Nothing


--

getTypeShape :: Type -> IO ( [(Unique, Unique)], Type )
getTypeShape t = do
  let (FreeVars ts ss bs rs) = freevars t --(trace ("t = " ++ show t ) t )
  
  -- generate \alpha substitution, and save the pairing alpha_0j alpha_ij
  alphaPairing <- mapM (\x -> do x' <- newUnique; return ( x, x' ) ) ( Set.toList ts )
  let alphaSub  = Map.fromList ( map ( Control.Arrow.second (TV . Alpha) ) alphaPairing )
  
  -- generate \psi substitution
  sesPairing  <- mapM (\x -> do x' <- newUnique; return ( x, x' ) ) ( Set.toList ss)
  let sesSub  = Map.fromList ( map ( Control.Arrow.second (SV . Psi) ) sesPairing )
  
  -- generate \beta substitution
  betaPairing  <- mapM (\x -> do x' <- newUnique; return ( x, x' ) ) ( Set.toList bs)
  let betaSub  = Map.fromList betaPairing

  -- generate \kappa substitution
  rhoPairing  <- mapM (\x -> do x' <- newUnique; return ( x, x' ) ) ( Set.toList rs)
  let rhoSub  = Map.fromList rhoPairing
  
  let typeShapeSubst = Sub alphaSub sesSub betaSub rhoSub Map.empty
  
  return ( alphaPairing, apply typeShapeSubst t) 


varToNode :: Unique -> LNode Unique
varToNode i = (hashUnique i, i) 

varsToEdge :: Node -> Node -> LEdge ()
varsToEdge n1 n2 = (n1, n2, ())

findRenaming  :: Alpha -> Type -> TVarEquivalence -> IO ( Maybe (Substitution, TVarEquivalence) )
findRenaming (Alpha i) t g = do
  let 
      freevarsT         = freevars t
      typeFreevars      = tvars freevarsT
      reachableAlphas   = reachable ( hashUnique i ) g
      aliasAlphas       = Set.fromList ( mapMaybe ( lab g ) reachableAlphas )
      diffAlphas        = Set.toList $ typeFreevars `Set.intersection` aliasAlphas
  --trace ("alpha = a" ++ show i ++ "\t\t type t = " ++ show t ++  "\nStarting graph: " ++ graphviz' g ++ "\naliases of alpha:" ++ show (Set.toList aliasAlphas) ++ "\tfree variables of type t: " ++ show (Set.toList typeFreevars) ++ " \t diff:" ++ show diffAlphas) (return ())
  if null diffAlphas 
    then
      do
        let alphas = Set.toList aliasAlphas
        pairingsAndTypeShapes <- mapM ( \_ -> getTypeShape t ) [1.. length alphas ]
        --pairingsAndTypeShapes <- mapM ( \_ -> getTypeShape t ) [1.. ( trace ("length = " ++ show (length alphas) ++ " alphas = "  ++ show alphas) (length alphas))]
        let pairings    = concatMap fst pairingsAndTypeShapes
        --let pairings    = concatMap fst ( trace ("pair+type = " ++ show pairingsAndTypeShapes) pairingsAndTypeShapes)
        
            typeShapes  = map snd pairingsAndTypeShapes
            alphaSubst  = Map.fromList ( zip alphas typeShapes ) 
            mainSubst   = Sub alphaSubst Map.empty Map.empty Map.empty Map.empty
            newLNodes   = concatMap (\(x, y) -> [varToNode x, varToNode y] ) pairings
            newLabels   = map (\( n1, n2 ) -> varsToEdge ( hashUnique n1 ) ( hashUnique n2 ) ) pairings
            newLoops    = map (\(n, _) -> varsToEdge n n ) newLNodes
            g'          = foldr (\n gr -> delNode ( hashUnique n ) gr ) g alphas
--            g'          = foldr (\n gr -> delNode ( trace ("del = " ++ show n)( hashUnique n )) gr ) g alphas
            g''         = insNodes ( filter (\(x, _) -> (not (gelem x g'))) newLNodes) g'
--            g''         = insNodes (trace ( "newnodes = " ++ show newLNodes)  ( filter (\(x, _) -> (not (gelem x g'))) newLNodes) ) g'
            g'''        = insEdges ( newLabels ++ newLoops ) g''
--            g'''        = insEdges ( trace ("new edges = " ++ show newLabels ++ " " ++ show newLoops ) (newLabels ++ newLoops) ) g''
        --trace ("w3: subst = " ++ show mainSubst ++ " \ng''' = " ++graphviz' g''' ) (return ())
        return ( Just ( mainSubst, undir g''' ) )
    else 
--      do
        return Nothing
        --trace "w4" (return Nothing)



findNonSubstitutableAlphas :: [Constraint] -> [Unique]
findNonSubstitutableAlphas ( _ : cs)              = findNonSubstitutableAlphas cs
findNonSubstitutableAlphas []                     = []


forceMatch' :: Substitution -> Alpha -> Type -> TVarEquivalence -> [Constraint] -> IO ( Maybe ( Substitution, TVarEquivalence ) )
forceMatch' sigma alpha t g cs = do
--      trace ("w2: t = " ++ show t ++ " a = " ++ show alpha) (return ())
--      trace ("Constraints = " ++ show cs) (return ()) 
      maybeMatch <- findRenaming alpha t g
      case maybeMatch of
        Nothing -> forceMatch sigma g cs
        Just ( sigma', g' ) ->  return $ Just (sigma', g' )


forceMatch :: Substitution -> TVarEquivalence -> [Constraint] -> IO ( Maybe ( Substitution, TVarEquivalence ) )
forceMatch sigma g ( TC t      (TV i) :cs )  = forceMatch' sigma i t g cs 
forceMatch sigma g ( TC (TV i) t      :cs )  = forceMatch' sigma i t g cs
--  ( Alpha _ )   -> forceMatch sigma g cs
--  _            -> forceMatch sigma g ( TC t1 t2 : cs )
forceMatch sigma g (_:cs)                   = forceMatch  sigma g cs 
forceMatch _     _ []                       = return Nothing





doAlgorithmF :: Substitution -> TVarEquivalence -> [Constraint] -> IO (Substitution, TVarEquivalence, Constraints)
doAlgorithmF sigma g cs = do
  let res = breakConstraints cs   
  case res of 
    Just cs1 -> doAlgorithmF sigma g cs1
--    Nothing  -> case breakTChans cs of 
--      Just (sigma', cs')  -> doAlgorithmF (sigma `compose` sigma') g cs'
    Nothing -> do
--        trace ("w1: cs == " ++ show cs ) (return ())
        mrResult <- forceMatch sigma g cs
        case mrResult of
          Just ( sigma', g' ) -> doAlgorithmF (sigma `compose` sigma') g' ( map (apply sigma') cs )
          Nothing   -> return (sigma, g, Set.fromList (map (apply sigma) cs))


algorithmF :: Constraints -> IO ( Substitution, Constraints )
algorithmF cs = do
  let FreeVars ts _ _ _ = freevars cs
      ns = map ( \tid -> (hashUnique tid, tid) ) ( Set.toList ts )
      graph = undir ( mkGraph ns [] )
  (subst, _, cs') <- doAlgorithmF idSub graph ( Set.toList cs )
  return (subst, cs')