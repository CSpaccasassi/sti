
module AlgorithmW where

import Syntax
import Types
import AlgorithmF
import AlgorithmR

import Data.Unique
import qualified Data.Set as Set

--import Debug.Trace (trace)
--import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import Text.PrettyPrint.HughesPJ ( render )
import Control.Arrow (second)
--import Prelude hiding (pure)


type Conf = ( Substitution, Type, Behav, Constraints )


-- note: the BW rules for behaviour and region constraints are ignored here, since
--       they do not contribute to the definition of Dangerous Variables later
expandConstraints :: [Constraint] -> [Constraint]
expandConstraints ( c@(TC ( TBFun _ _ b) ( TBFun _ _ b')) : cs )
                            = BC (BV b) b' : c : expandConstraints cs
--expandConstraints ( c@(TC ( TChan a r) (TChan a' r')) : cs )
--                            = TC a a'
--                            : TC a' a
--                            : RC r r'
--                            : c : expandConstraints cs
expandConstraints ( c@(TC (TPair t1 t2) (TPair t1' t2')) : cs)
                            = TC t1 t1'
                            : TC t2 t2'
                            : c : expandConstraints cs
expandConstraints ( c : cs ) = c : expandConstraints cs
expandConstraints []         = []


isTC :: Constraint -> Bool
isTC ( TC _ _ ) = True
isTC _          = False


backwardClosure :: Constraints -> Constraints
backwardClosure cs =
  let csl  = Set.toList cs
      csl'    = expandConstraints csl
      typeCs = filter isTC csl'
      tyClos = map (\t ->
          let doTransitivity t1 t2 =
                let doTransitivity2 t2' t3' = if t2 == t2'
                                            then Just (TC t1 t3')
                                            else Nothing
                in mapMaybe (\t' -> case t' of
                        TC t2' t3  -> doTransitivity2 t2' t3
                        _         -> Nothing
                        ) typeCs
          in case t of
                 TC t1 t2   -> doTransitivity t1 t2
                 _         -> []
                  ) typeCs
      csl''   = concat tyClos ++ csl'
      cs'     = Set.fromList csl''
  in if Set.size cs == Set.size cs'
      then cs'
      else backwardClosure cs'


bwArrow :: Unique -> Constraint -> FreeVars
bwArrow i c@( BC _ (Beta j) ) =
    if i == j
      then freevars c
      else mkBFV [i]
bwArrow i _  = mkBFV [i]


downwardClosure :: FreeVars -> Constraints -> FreeVars
downwardClosure fv@(FreeVars _ _ bs _ ) cs =
    let fv' = Set.fold (\x acc ->
                          Set.fold (\y acc' -> bwArrow x y `unionFV` acc')
                                   emptyFreeVars
                                   (
                                   --trace ("bw clos = " ++ show ( backwardClosure cs)) $
                                   backwardClosure cs
                                   ) --TODO: is bw closure correct?
                             `unionFV` acc)
                       fv
                       bs
    in if sizeFV fv == sizeFV fv'
          then fv'
          else downwardClosure  fv' cs


getClos' :: FreeVars -> [Constraint] -> FreeVars
getClos' fv (c:cs) = if any (\z -> z `fvMember` freevars c) (toListFV fv)
                      then getClos' (fv `unionFV` freevars c) cs
                      else getClos' fv cs
getClos' fv [] = fv

getClos :: FreeVars -> [Constraint] -> FreeVars
getClos fv cs = let fv' = getClos' fv cs
                in if sizeFV fv == sizeFV fv'
                    then fv'
                    else getClos fv' cs


generalize :: TypingContext -> Behav -> Exp -> Constraints -> Type -> TypeSchema--(Type, FreeVars, Constraints)
generalize tc b e cs t =
  let fvType           = freevars t
      dangerousVars    = freevars tc `unionFV` freevars b `unionFV` freevars e

      --`unionFV` freevars (Set.filter (\z -> case z of
--                                                                                                      NTC _ _ -> True
--                                                                                                      _       -> False) cs)
      universe         = getClos fvType ( Set.toList cs )
      nonGeneralizable = downwardClosure dangerousVars cs
      gamma            = (universe `diffFV` nonGeneralizable)
      gammas           = mkGammas gamma
      cs'              = Set.filter (\c -> sizeFV ( freevars c `intersectionFV` gamma ) /= 0 ) cs
  in (TS gammas cs' t)--t, gamma,
--          trace ("Generalized:\n" ++
--          "tc: "        ++ show tc ++ "\n" ++
--          "b: "         ++ show b ++ "\n" ++
--          "C: "         ++ show cs ++ "\n" ++
--          "t: "         ++ show t ++ "\n" ++
--          "universe: "  ++ show universe ++ "\n"  ++
--          "DV: "        ++ show dangerousVars ++ "\n"  ++
--          "NG: "        ++ show nonGeneralizable ++ "\n"  ++
--          "gammas = "   ++ show gamma ++ "\n" ++
--          "C0: "        ++ show cs' ++ "\n"
--          )
--          cs')


--------------------------------------------------------------------------------
-- Type inference
--------------------------------------------------------------------------------

--refreshLabels :: Constraint -> IO Constraint
--refreshLabels (RC (RLabel _) r2 ) = do
--  l <- newUnique
--  return (RC (RLabel l) r2)

ctxLookup :: TypingContext -> Int -> IO Conf
ctxLookup tctx i = case tctx !! i of
  TyVar _ t  -> return (idSub, t, BTau, Set.empty)
  TSVar _ ts -> freshInstanceTS ts



simpleTypeConf :: Type -> Conf
simpleTypeConf t = (idSub, t, BTau, Set.empty )

typeOfVal :: TypingContext -> Val -> IO Conf
typeOfVal tc v = case v of
  U            -> return ( simpleTypeConf TUnit )
  TT           -> return ( simpleTypeConf TBool )
  FF           -> return ( simpleTypeConf TBool )
  N _          -> return ( simpleTypeConf TInt  )

  PairV v1 v2   -> do
      a0  <- newAlphaType
      a1  <- newAlphaType
      a2  <- newAlphaType
      b   <- newBeta
      let t0 = TBFun a1 ( TBFun a2 ( TPair a1 a2) b) b
      (s1, t1, b1, c1 ) <- algorithmW tc            ( V v1 )
      (s2, t2, b2, c2 ) <- algorithmW (apply s1 tc) ( V v2 )

      let s'  = s1 `compose` s2
          t0' = ( apply s2 . apply s1 )  t0
          t'  = TBFun (apply s2 t1) (TBFun t2 a0 b) b
          b'  = BSeq (apply s2 b1) b2
          c1' = Set.map (apply s1) c1
          c3  = [TC t0' t', BC BTau b]
          c'  = c1' `Set.union` c2 `Set.union`Set.fromList c3
      return (s', a0, b', c' )

  Fun x e -> do
      -- new vars
      alpha1 <- newAlphaType
      alpha2 <- newAlphaType
      beta   <- newBeta

      -- type/effect inference on body
      let funType = TBFun alpha1 alpha2 beta
          argCtx  = TyVar x alpha1
          tc'     = argCtx  : tc
      (s', t', b', c' ) <- algorithmW tc' e

      -- result
      let t'' = apply s' funType
          c1  = BC b' ( apply s' beta )
          c2  = TC t' ( apply s' alpha2 )
          c'' = c' `Set.union` Set.fromList [c1, c2]
      return (s', t'',  BTau, c'' )

  Fix f x e -> do
      -- new vars
      a1 <- newAlphaType
      a2 <- newAlphaType
      beta   <- newBeta

      -- type/effect inference on body
      let funType = TBFun a1 a2 beta
          funCtx  = TyVar f funType
          argCtx  = TyVar x a1

          tc'     = funCtx : argCtx  : tc
      (s', t', b', c' ) <- algorithmW tc' e
      if pureExp e b'
        then do
          -- result
          let t'' = apply s' funType
              beta' = apply s' beta
              c1  = BC (BRec b' beta') beta'
              c2  = TC t' ( apply s' a2 )
              c'' = c' `Set.union` Set.fromList [c1, c2]
          return (s', t'',  BTau, c'' )
        else error $ "Impure typing context in " ++ show v

--  PubChan c _ -> error $ "Public channel " ++ c ++ " used as value"
  Ses       r -> return ( simpleTypeConf (TSes r) )

inferConstruct1 :: TypingContext -> Type -> [Constraint] -> Exp ->
  IO (Substitution, Type, Behav, Constraints )
inferConstruct1 tc t0 c0 e1 = do
      (s1, t1, b1, c1 ) <- algorithmW tc e1
      alpha <- newAlphaType
      beta  <- newBeta
      let c0' = map (apply s1) c0
          t0' = apply s1 t0
          t'  = TBFun t1 alpha beta
          c2  = TC t0' t'
--          c3  = BC b1 beta
          c'  = Set.fromList c0' `Set.union` c1 `Set.union` Set.singleton c2
      return (s1, alpha, BSeq b1 (BV beta), c' )

inferConstruct2 :: TypingContext -> Type -> [Constraint] -> Exp -> Exp ->
  IO (Substitution, Type, Behav, Constraints )
inferConstruct2 tc t0 c0 e1 e2 = do
      alpha <- newAlphaType
      beta1 <- newBeta
      beta2 <- newBeta
      (s1, t1, b1, c1 ) <- algorithmW tc e1
      (s2, t2, b2, c2 ) <- algorithmW (apply s1 tc) (apply s1 e2)
      let s'  = s1 `compose` s2
          b'  = BSeq (apply s2 b1) ( BSeq b2 (BSeq (BV beta1) (BV beta2) ) )
          c0' = map (apply s2 . apply s1) c0
          t0' = ( apply s2 . apply s1 )  t0
          c1' = Set.map (apply s1) c1
          t'  = TBFun (apply s2 t1) (TBFun t2 alpha beta2) beta1
          c3  = TC t0' t'
--          c4  = BC b1 beta1
--          c5  = BC b2 beta2
          c'  = Set.fromList c0' `Set.union` c1' `Set.union` c2 `Set.union` Set.singleton c3--, c4, c5]
      return (s', alpha, b', c' )

inferConstruct2tau :: TypingContext -> Type -> [Constraint] -> Exp -> Exp ->
  IO (Substitution, Type, Behav, Constraints )
inferConstruct2tau tc t0 c0 e1 e2 = do
      beta  <- newBeta
      alpha <- newAlphaType
      (s1, t1, b1, c1 ) <- algorithmW tc e1
      (s2, t2, b2, c2 ) <- algorithmW (apply s1 tc) (apply s1 e2)
      let s'  = s1 `compose` s2
          b'  = BSeq (apply s2 b1) b2
          c0' = map (apply s2 . apply s1) c0
          c1' = Set.map (apply s1) c1
          t0' = ( apply s2 . apply s1 )  t0
          t'  = TBFun (apply s2 t1) (TBFun t2 alpha beta) beta
          c3  = [TC t0' t', BC BTau beta]
          c'  = Set.fromList c0' 
                  `Set.union` c1' 
                  `Set.union` c2 
                  `Set.union` Set.fromList c3
      return (s', alpha, b', c' )


algorithmW' :: TypingContext -> Exp -> IO (Substitution, Type, Behav, Constraints )
algorithmW' tc expr = case expr of
  -- values
  V v        -> typeOfVal tc v
  Free x     -> error $ "Algorithm W' encountered free variable" ++ x
  Bound _ i  -> ctxLookup tc i

  -- tuples
  Pair e1 e2       -> do
    alpha1 <- newAlphaType
    alpha2 <- newAlphaType
    beta   <- newBeta
    let tupType = TBFun alpha1 ( TBFun alpha2 ( TPair alpha1 alpha2) beta) beta
    ( s', t', b', c' ) <- inferConstruct2tau tc tupType [BC BTau beta] e1 e2

    return ( s', t',  b', c' )



  -- lambda terms
  App e1 e2       -> do
    alpha <- newAlphaType
    beta  <- newBeta
    (s1, t1, b1, c1 ) <- algorithmW tc e1
    (s2, t2, b2, c2 ) <- algorithmW ( apply s1 tc ) ( apply s1 e2 )
    let s' = s1 `compose` s2
        b1' = apply s2 b1
        b'  = BSeq b1' (BSeq b2 (BV beta))
        c1' = Set.map (apply s2) c1
        c3  = TC ( apply s2 t1 ) (TBFun t2 alpha beta)
        c'  = c1' `Set.union` c2 `Set.union` Set.singleton c3

    return (s', alpha, b', c' )


  Let x e1 e2     -> do
    (s1, t1, b1, c1 ) <- algorithmW tc e1
    let tc' = apply s1 tc
    let ts@(TS _ genC1 _) = generalize tc' b1 e1 c1 t1
    (s2, t2, b2, c2 ) <- algorithmW ( TSVar x ts:tc' ) (apply s1 e2)
    let s'  = s1 `compose` s2
        b1' = apply s2 b1
        b'  = BSeq b1' b2
        c1' = apply s2 ( c1 `Set.difference` genC1 )
        c'  = c1' `Set.union` c2
    return  (s', t2, b', c' )


  If e0 e1 e2     -> do
    (s0, t0, b0, c0 ) <- algorithmW tc e0
    (s1, t1, b1, c1 ) <- algorithmW ( apply s0 tc ) e1
    (s2, t2, b2, c2 ) <- algorithmW ( apply s1 . apply s0 $ tc ) e2
    alpha <- newAlphaType

    let s' = s0 `compose` ( s1 `compose` s2 )
        b0' = apply s2 ( apply s1 b0 )
        b1' = apply s2 b1
        b'  = BSeq b0' ( BIn b1' b2)
        c0' = Set.map (apply s2 . apply s1) c0
        c1' = Set.map (apply s2) c1
        c4  = TC ( apply s2 ( apply s1 t0 ) ) TBool
        c5  = TC ( apply s2 t1 ) alpha
        c6  = TC t2 alpha
        c'  = c0' `Set.union` c1' `Set.union` c2 `Set.union` Set.fromList [c4, c5, c6 ]
    return (s', alpha, b', c' )

  {- The following two cases mimick the req-c/acc-c construct -}
  Req c l -> do
      psi <- newPsi
      rho  <- newRho
      beta <- newBeta

      let sc = SC (SV psi) (SReq c l)
          bc = BC (BPush l (SV psi)) beta
          rc = RC (RL l) rho
          t  = TBFun TUnit (TSes (RV rho)) beta
      return (idSub, t, BTau, Set.fromList [bc, sc, rc])-- [kc, bc, sc])

  Acc c l -> do
      rho  <- newRho
      psi <- newPsi
      beta <- newBeta

      let sc = SC (SV psi) (SAcc c l)
          rc = RC (RL l) rho
          bc = BC (BPush l (SV psi)) beta
          t  = TBFun TUnit (TSes (RV rho)) beta
      return (idSub, t, BTau, Set.fromList [bc, sc, rc])-- [kc, bc, sc])

  {- The result of each sub-case of a constant c, is the application of its type
     schema to the INST function on fig. 4.3, p.103, N. & N.  -}
  C c        -> case c of
--    Request -> do
--      Psi i  <- newPsi
--      psi' <- newPsi
--      rho  <- newRho
--      beta <- newBeta
--
--      let varpsi = SReq (SL i)
--
--
--      let sc = SC (SV psi') varpsi
--          bc = BC (BPush (RV rho) (SV psi')) beta
--          t  = TBFun (TChan (SL i)) (TSes (RV rho)) beta
--      return (idSub, t, BTau, Set.fromList [bc, sc])-- [kc, bc, sc])
--
--    Accept -> do
--      Psi i  <- newPsi
--      psi'   <- newPsi
--      beta   <- newBeta
--      rho    <- newRho
--
--      let barpsi = SAcc (SL i)
--
--      let sc = SC (SV psi') barpsi
--          bc = BC (BPush (RV rho) (SV psi')) beta
--          t  = TBFun (TChan (SL i)) (TSes (RV rho)) beta
--      return (idSub, t, BTau, Set.fromList [bc, sc])

    Send -> do
      beta  <- newBeta
      rho   <- newRho
      alpha <- newAlpha

      let bc = BC (BOp (BSend (RV rho) (TV alpha))) beta
          t  = TBFun (TPair (TSes (RV rho)) (TV alpha)) TUnit beta
      return (idSub, t, BTau, Set.fromList [bc])

    Recv  -> do
      alpha <- newAlpha
      beta  <- newBeta
      rho   <- newRho

      let bc = BC (BOp (BRecv (RV rho) (TV alpha))) beta
          t  = TBFun (TSes (RV rho)) (TV alpha) beta
          --ts = TS [SG barpsi, SG psi', BG beta, KG rho] [kc, bc, sc] t
      return (idSub, t, BTau, Set.fromList [bc])


    Delegate -> do
      rho   <- newRho
      rho'  <- newRho
      beta  <- newBeta

      let bc = BC (BOp (BDeleg (RV rho) (RV rho'))) beta
          t  = TBFun (TPair (TSes (RV rho)) (TSes (RV rho'))) TUnit beta
      return (idSub, t, BTau, Set.fromList [bc])

    Resume l -> do
      beta  <- newBeta
      rho   <- newRho

      let bc = BC (BOp (BResume (RV rho) (RL l))) beta
          t  = TBFun (TSes (RV rho)) (TSes (RL l)) beta
          --ts = TS [SG barpsi, SG psi', BG beta, KG rho] [kc, bc, sc] t
      return (idSub, t, BTau, Set.fromList [bc])

    Fst  -> do
      alpha1 <- newAlpha
      alpha2 <- newAlpha
      beta  <- newBeta

      let bc = BC BTau beta
          t  = TBFun (TPair (TV alpha1) (TV alpha2)) (TV alpha1) beta
          --ts = TS [SG barpsi, SG psi', BG beta, KG rho] [kc, bc, sc] t
      return (idSub, t, BTau, Set.fromList [bc])

    Snd-> do
      alpha2 <- newAlpha
      alpha1 <- newAlpha
      beta  <- newBeta

      let bc = BC BTau beta
          t  = TBFun (TPair (TV alpha1) (TV alpha2)) (TV alpha2) beta
          --ts = TS [SG barpsi, SG psi', BG beta, KG rho] [kc, bc, sc] t
      return (idSub, t, BTau, Set.fromList [bc])


  Select e l -> do
      rho  <- newRho
      beta <- newBeta
      let selectType = TBFun (TSes (RV rho)) TUnit beta
          c0         = BC (BOp (BInC (RV rho) l)) beta
      inferConstruct1 tc selectType [c0] e

  Match e es -> do
      -- analyse e
      (s_0, t_0, b_0, cs_0) <- algorithmW tc e

      -- e must be a session end-point
      rho  <- newRho
      let c0'   = TC t_0 (TSes (RV rho))
          cs_0' = Set.insert c0' cs_0
          tc_0' = apply s_0 tc

      -- analyse Match's arguments
      let f tc_i es_i s_i ts_i bs_i cs_i =
            case es_i of
              []            -> return (s_i, ts_i, bs_i, cs_i)
              (l_i, e_i):es'   -> do
                (s_i', t', b', c') <- algorithmW tc_i (apply s_i e_i)
                let s' = s_i `compose` s_i'
                    ts' = map (apply s_i') ts_i ++ [t']
                    bs' = map ( second (apply s_i') ) bs_i ++ [(l_i, b')]
                    cs' = apply s_i' cs_i `Set.union` c'
                    tc'  = apply s_i' tc_i
                f tc' es' s' ts' bs' cs'

      (s_n, ts, bs, cs_n) <- f tc_0' es s_0 [] [] cs_0'

      alpha <- newAlpha
      let tcs = map (\t -> TC t (TV alpha) ) ts
      let cs' = foldr Set.insert cs_n tcs
      return (s_n, TV alpha, BSeq b_0 (BEx (RV rho) bs), cs')

  Spawn e    -> do
    beta <- newBeta
    alpha <- newAlpha
    (s, t, b, c ) <- algorithmW tc e
    let c2  = TC (TBFun TUnit (TV alpha) beta) t
        c'  = Set.insert c2 c
    return (s, TUnit, BSeq b (BSpawn (BV beta)), c')

pureExp :: Exp -> Behav -> Bool
pureExp e b = let ks_b = prjKappas (freevars b)
                  ks_e = prjKappas (freevars e)
              in Set.null (Set.intersection ks_e ks_b)


algorithmW :: TypingContext -> Exp -> IO (Substitution, Type, Behav, Constraints)
algorithmW tc e = do
--  trace ("Step W : $ " ++ show e ++ " $\\\\\n typing context: $ "
--                       ++ showTC tc ++ "$ \n\\\\ --------------\\\\") (return () )

  ( s1, t1, b1, c1 ) <- algorithmW' tc e                     -- get program type

--  trace ("Step W': $ " ++ show e
--          ++ " $\\\\\n  type: $ " ++ show t1
--          ++ " $ \n\\\\  beha: $ " ++ show b1
--          ++ " $ \n\\\\  cons: " ++ (render . ppConstraints) c1
--          ++ " \n  subs: " ++ show s1
--          ++ " \n \\\\--------------\\\\ ") (return () )

  ( s2, c2 ) <- algorithmF c1                             -- do algorithm F

--  trace ("Step F: $ " ++ show e ++ " $\n \\\\ cons: " ++ (render . ppConstraints) c2
--                              ++ "\n subs: " ++ show s2
--                              ++ "\n  \\\\--------------\\\\ ") (return () )
  let t2  = apply s2 t1
      b2  = apply s2 b1
      tc2 = (apply s2 . apply s1) tc
  let ( c3, t3, b3 ) = algorithmR tc2 ( c2, t2, b2 )        --do algorithm R

--  trace ("Step R: $ " ++ show e
--          ++ " $\\\\\n  type: $ " ++ show t3
--          ++ " $ \n\\\\  beha: $ " ++ show b3
--          ++ " $ \n\\\\  cons: " ++ (render . ppConstraints) c3
--          ++ " \n  --------------\\\\ ") (return () )

  return ( s1 `compose` s2, t3, b3, c3 )
