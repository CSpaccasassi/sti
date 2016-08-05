{-# LANGUAGE TypeSynonymInstances #-}

module AlgorithmST where

--import AlgorithmF
--import AlgorithmR
import Data.List
import Data.Unique
import Syntax
import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ ( text, (<>),  Doc, render )
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Control.Arrow (second)
type SesEndpoint = Unique

type Solution = (Substitution, Constraints)
type DFrame = (R, Ses)
data Delta = D{fr ::[DFrame],
               lb :: Set.Set R } -- the set of already pushed labels

instance Show Delta where show = render . ppDelta

emptyDelta :: Delta
emptyDelta = D [] Set.empty

ppDelta :: Delta -> Doc
ppDelta D {fr = []}              = text "\\epsilon"
ppDelta D {fr = (r, s):delta, lb = ls} = text "( " <> ppRL r
                              <> text " : "
                              <> ppSession s <> text " ) \\cdot "
                              <> ppDelta (D delta ls)

instance Substitutable Delta where
  apply s d@D {fr = fs}   = d{fr = map (second . apply $ s) fs}
  freevars  D {fr = fs} = foldr
                              (\(_, s) acc -> acc `unionFV` freevars s)
                              emptyFreeVars
                              fs

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

isTransitive :: Reg -> Reg -> [Constraint] -> [Constraint] -> Bool
isTransitive r1 r2 _ [] = r1 == r2
isTransitive r1 r2 cs_orig (RC r r':cs) = r1 == r2 ||
  if RV r' == r2
    then isTransitive r1 r cs_orig cs_orig
    else
  if r  == r1
    then isTransitive (RV r') r2 cs_orig cs
    else isTransitive r1 r2 cs_orig cs

isTransitive r1 r2 cs_orig (_ : cs) = isTransitive r1 r2 cs_orig cs

sameRegion :: Reg -> Reg -> Constraints -> Bool
sameRegion k1 k2 cs = isTransitive k1 k2 (Set.toList cs) (Set.toList cs)


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Simplify input behaviour
-- -----------------------------------------------------------------------------

-- creates a substitution from region variables to region labels; also
-- verifies endpoint aliasing (no constraint C |- l1 ~ l2 is derivable)
removeRegs :: Constraints -> Map.Map Rho Reg
removeRegs cs = regSub
  where
   rSub = Set.fold (\c sigma -> case c of
    RC (RV r1@(Rho _)) r2   -> let Rho r1' = apply sigma r1
                                   Rho r2' = apply sigma r2
                               in sigma `compose` mkRSub r1' r2'

    _                       -> sigma
    ) idSub cs
   cs' = Set.filter (\c -> case c of
                              RC (RL _) _ -> True
                              _ -> False)
                (apply rSub cs)

   regSub = Set.fold (\c m -> case c of
      RC l r -> Map.insertWith'
                  (\l1 l2 -> if l1 == l2
                               then l1
                               else error $ "Endpoint aliasing detected: "
                                            ++ show l1 ++ " ~ " ++ show l2
                  ) r l m
      _ -> error "unexpected constraint"
    ) Map.empty cs'

-- substitutes all region variables in b with a concrete label;
-- prepares recursive loops in b; substitutes all betas with concrete behaviours
-- removes taus
simplifyBehaviour :: Constraints -> Behav -> Behav
simplifyBehaviour cs b = case b of
  BV beta -> let bs' = Set.fold (\c bs -> case c of
                                        BC b' beta' -> if beta == beta'
                                                         then b':bs
                                                         else bs
                                        _ -> bs) [] cs
             in if null bs'
                  then error $ "unbound beta variable " ++ show b
                  else simplifyBehaviour cs (foldl1 BIn bs')
  BTau    -> BTau
  BPush   r s -> BPush r s

  -- b. ops
  BOp (BSend   r t  ) -> BOp (BSend (lookupReg r) t)
  BOp (BRecv   r t  ) -> BOp (BRecv (lookupReg r) t)
  BOp (BDeleg  r r' ) -> BOp (BDeleg (lookupReg r) (lookupReg r'))
  BOp (BResume r l  ) -> BOp (BResume (lookupReg r) l)
  BOp (BInC    r lab) -> BOp (BInC    (lookupReg r) lab)

  -- b. composition
  BSeq    BTau b2  -> simplifyBehaviour cs b2
  BSeq    b1 BTau  -> simplifyBehaviour cs b1
  BSeq    b1 b2    -> let b1' = simplifyBehaviour cs b1
                          b2' = simplifyBehaviour cs b2
                          b'  = BSeq b1' b2'
                      in if b1' == BTau || b2' == BTau
                          then simplifyBehaviour cs b'
                          else b'
  BRec    b1 beta1 -> let prepRec = Set.map
                           (\c -> case c of
                                        BC b2 beta2 ->  if beta1 == beta2
                                          then case b2 of
                                            BRec _ _ -> BC BTau beta1
                                            _        -> error "non-rec in C"
                                          else c
                                        _ -> c
                            )
                          b1' = simplifyBehaviour (prepRec cs) b1
                      in BRec b1' beta1
  BSpawn  b'       -> BSpawn (simplifyBehaviour cs b')
  BIn     b1 b2    -> BIn (simplifyBehaviour cs b1) (simplifyBehaviour cs b2)
  BEx     r lbs    -> BEx (lookupReg r)
                       (map (second (simplifyBehaviour cs)) lbs)
 where
      m = removeRegs cs
      lookupReg r1 = case r1 of
                     RV r2 -> fromMaybe r1 (Map.lookup r2 m)
                     _     -> r1
-- -----------------------------------------------------------------------------
-- Simplify constraints
-- -----------------------------------------------------------------------------
simplifyConstraints :: Constraints -> Substitution
simplifyConstraints c0 = fst res
  where res = Set.fold (\co (s0, m0) ->
            case co of
                  SC (SV (Psi psi)) (SReq c _) -> doLookup m0 s0 psi (c, True)
                  SC (SV (Psi psi)) (SAcc c _) -> doLookup m0 s0 psi (c, False)
                  _  -> (s0, m0)
              ) (idSub, Map.empty) c0
        doLookup m0 s0 psi chan = case Map.lookup chan m0 of
                          Nothing   -> let m1 = Map.insert chan psi m0
                                       in (s0, m1)
                          Just psi' -> let s1 = mkSesSub psi (SV $ Psi psi')
                                       in (s1, m0)
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- folds the input 'b's into an internal choice
createInChoice :: [Behav] -> Behav
createInChoice bs =  case bs of
  []  -> error "Creating empty internal choice"
  [b] -> b
  _   -> foldl1 BIn bs

-- retrieves the behaviours below i in cs
-- (i.e. the 'b's of the kind  (b \subseteq \beta_i) \in cs)
relatesBeta :: Beta -> [Constraint] -> [Behav]
relatesBeta _ [] = []
relatesBeta (Beta i) (c:cs) = case c of
  BC  b (Beta j) -> let bs = relatesBeta (Beta i) cs
                    in if i == j
                        then b:bs
                        else bs
  _              -> relatesBeta (Beta i) cs


--------------------------------------------------------------------------------

type Conf = (Delta, Behav)
type Kont = [Behav]

showConf :: Conf -> String
showConf (D fs _,b) =
  "(" ++ (if null fs
                  then "[]"
                  else concatMap show fs)
             ++ ", " ++ (render . ppBehav) b ++ ")"






closeFrame :: R -> Delta -> (Substitution, Delta)
closeFrame _ d@D {fr = []}             = (idSub, d)
closeFrame l d@D {fr = (l',ses) : fs}  =
  if l == l'
  then let d' = d{fr = fs} in
       case ses of
        SEnd         -> (idSub, d')
        SV(Psi svar) -> let s1 = mkSesSub svar SEnd
                        in (s1, apply s1 d')
        _            -> error "Cannot close frame"
  else let (s1, d1) = closeFrame l d{fr = fs}
           fs'      = (l', apply s1 ses):fr d1
       in (s1, d1{fr = fs'})


-- ----------------------------------------------------------------------------
-- Algorithm MC
-- ----------------------------------------------------------------------------
algoMC :: Conf -> Kont -> Constraints -> IO Solution
algoMC c k c0 = do trace (showConf c) (return ()) -- debugging trace
                   algoMC_ c k c0  -- algoMC_ implements MC

-- ----------------------------------------------------------------------------
-- Base cases
-- ----------------------------------------------------------------------------
algoMC_ :: Conf -> Kont -> Constraints -> IO Solution

-- remove terminated sessions
algoMC_ (d@D{fr=(_,SEnd):fs}, b) k0 c0 = algoMC (d{fr=fs}, b) k0 c0

-- Termination
algoMC_ (d, BTau)   [] c0 = let s1 = finalize (freevars d)
                                c1 = apply s1 c0
                            in return (s1, c1)

-- Continuation
algoMC_ (d, BTau) (k:ks)  c0 = algoMC (d, k) ks c0

-- ----------------------------------------------------------------------------
-- Push
-- ----------------------------------------------------------------------------
algoMC_ (d, BPush l s) k0 c0 =
  if l `Set.member` lb d
    then error "pushing label twice on delta"
    else do
      let (s1, D fs ls) = closeFrame l d
          d1       = D ((l, apply s1 s):fs) (Set.insert l ls)
          k1       = map (apply s1) k0
          c1       = apply s1 c0
      algoMC (d1, BTau) k1 c1

-- ----------------------------------------------------------------------------
-- Send
-- ----------------------------------------------------------------------------
algoMC_ (d@D{fr = (l, SV (Psi psi)) : fs}, BOp (BSend   (RL l1) t)) k0 c0
  | l1 == l = do
    alpha <- newAlpha
    psi'  <- newPsi
    let t'    = TV alpha
        ses1  = SV psi'
        s1    = mkSesSub psi (SSend t' ses1)
        k1    = map (apply s1) k0
        d1    = d{fr = (l, ses1) : map (second (apply s1)) fs}
        c1    = Set.insert (TC (apply s1 t) t') (apply s1 c0)
    (s2, cs2) <- algoMC (d1, BTau) k1 c1
    return (s1 `compose` s2, cs2)

algoMC_ (d@D{fr = (l1, SSend t' ses1) : fs}, BOp (BSend (RL l1') t)) k0 c0 =
    if l1 /= l1'
      then error $ "Mismatch: send " ++ show l1 ++ " /= " ++ show l1'
      else do
    let c1   = Set.insert (TC t t') c0
    algoMC (d{fr = (l1, ses1) : fs}, BTau) k0 c1


-- ----------------------------------------------------------------------------
-- Recv
-- ----------------------------------------------------------------------------
algoMC_ (d@D{fr = (l1, SV (Psi psi)) : fs}, BOp (BRecv (RL l1') t)) k0 c0
 | l1 == l1' = do
    alpha <- newAlpha
    psi'  <- newPsi
    let t'    = TV alpha
        ses1  = SV psi'
        s1    = mkSesSub psi (SRecv t' ses1)
        k1    = map (apply s1) k0
        d1    = d{fr = (l1, ses1) : map (second (apply s1)) fs}
        c1   = Set.insert (TC t' (apply s1 t)) (apply s1 c0)
    (s2, cs2) <- algoMC (d1, BTau) k1 c1
    return (s1 `compose` s2, cs2)

algoMC_ (d@D{fr = (l1, SRecv t' ses1) : fs}, BOp (BRecv (RL l1') t)) k0 c0 =
    if l1 /= l1'
      then error $ "Mismatch: recv " ++ show l1 ++ " /= " ++ show l1'
      else do
    let c1   = Set.insert (TC t' t) c0
    algoMC (d{fr=(l1, ses1): fs}, BTau) k0 c1

-- ----------------------------------------------------------------------------
-- Delegate
-- ----------------------------------------------------------------------------
algoMC_ (d@ D{fr = (l1, SV (Psi psi)):(l2, ses2):fs},
                                        BOp (BDeleg  (RL l1') (RL l2'))) k0 c0
  | l1 == l1' && l2 == l2' = do
        psi1  <- newPsi
        let ses1 = SV psi1
            s1   = mkSesSub psi (SDeleg ses2 ses1)
            k1   = map (apply s1) k0
            c1   = apply s1 c0
            d1   = d { fr = (l1, ses1) : map (second (apply s1)) fs}
        (s2, cs2) <- algoMC (d1, BTau) k1 c1
        return (s1 `compose` s2, cs2)

algoMC_ (d@ D{fr = (l1, SDeleg dlg ses1):(l2, ses2):fs},
                                            BOp(BDeleg(RL l1')(RL l2'))) k0 c0
  | l1 == l1' && l2 == l2' = do
        let (s1,c0')  = sesSubtype ses2 dlg
            k1        = map (apply s1) k0
            d1        = d{fr = map (second (apply s1)) ((l1, ses1) : fs)}
            c1        = apply s1 c0 `Set.union` c0'
        (s2, c2) <- algoMC (d1, BTau) k1 c1
        return (s1 `compose` s2, c2)

algoMC_ (d@ D{fr = (l1, _):(l2, _):_}, b@(BOp(BDeleg(RL l1')(RL l2')))) k0 c0
 | l1 == l1' && l2 /= l2' = do
    let (s1, d1)  = closeFrame l2 d
        b1        = apply s1 b
        k1        = map (apply s1) k0
        c1        = apply s1 c0
    (s2, c2) <- algoMC (d1, b1) k1 c1
    return (s1 `compose` s2, c2)

-- ----------------------------------------------------------------------------
-- Resume
-- ----------------------------------------------------------------------------
algoMC_ (d@ D{fr = [(l1, SV (Psi psi))]},
                                        BOp (BResume (RL l1') (RL l2'))) k0 c0
 | l1 == l1' = do
      psi1  <- newPsi
      psi2  <- newPsi
      let ses1  = SV psi1
          ses2  = SV psi2
          s1    = mkSesSub psi (SResume ses2 ses1)
          k1    = map (apply s1) k0
          d1    = d{ fr = [(l1, ses1), (l2', ses2)] }
          c1    = apply s1 c0
      (s2, cs2) <- algoMC (d1, BTau) k1 c1
      return (s1 `compose` s2, cs2)

algoMC_ (d@ D{fr = [(l1, SResume rses ses)]},
                                        BOp (BResume (RL l1') (RL l2'))) k0 c0
 | l1 == l1' = algoMC_ (d{fr = [(l1, ses), (l2',  rses)]}, BTau) k0 c0

algoMC_ (d@ D{fr = (l1, SResume _ _): (l2, _) : _},
                                            b@(BOp (BResume (RL l1') _))) k0 c0
  | l1 == l1' =  do let (s1, d1) = closeFrame l2 d
                        k1       = map (apply s1) k0
                        b1       = apply s1 b
                        c1       = apply s1 c0
                    (s2, c2) <- algoMC (d1, b1) k1 c1
                    return (s1 `compose` s2, c2)

-- ----------------------------------------------------------------------------
-- Select
-- ----------------------------------------------------------------------------
algoMC_ (d@ D{fr = (l1, SV (Psi psi)) : fs}, BOp (BInC (RL l1') lab)) k0 c0
  | l1 == l1' = do
      ivar  <- newPsi
      psi' <- newPsi
      let ses1  = SV   psi'
          s1    = mkSesSub psi (IVar ivar)
          k1    = map (apply s1) k0
          d1    = d{fr = (l1, ses1) : map (second (apply s1)) fs}
          ic    = Map.singleton lab ses1
          c1    = IVarC ic ivar `Set.insert` apply s1 c0
      (s2, cs2) <- algoMC (d1, BTau) k1 c1
      return (s1 `compose` s2, cs2)


algoMC_ (d@ D{fr = (l1, IVar psiint) : fs}, BOp (BInC (RL l1') lab)) k0 c0
  | l1 == l1' =  do
      let ic = getIChoice c0 psiint
      case Map.lookup lab ic of
                Just ses1 -> algoMC (d{fr = (l1, ses1):fs}, BTau) k0 c0
                Nothing   -> do
                  psi <- newPsi
                  let ses1  = SV psi
                      ic'   = Map.insert lab (SV psi) ic
                      c1    = updateIVar psiint ic' c0
                      d1    = d{fr = (l1, ses1) : fs}
                  algoMC (d1, BTau) k0 c1



-- ----------------------------------------------------------------------------
-- External choice
-- ----------------------------------------------------------------------------
algoMC_ (d@ D{fr = (l1, SV (Psi psi)) : fs}, b@(BEx (RL l1') bs)) k0 c0
  | l1 == l1' = do
      evar <- newPsi
      psis <- mapM (const (do x <- newPsi
                              return (SV x))) bs
      let s1 = mkSesSub psi (EVar evar)
          ec = Map.fromList (zip (map fst bs) psis)
          c1 = EVarC ec Map.empty evar `Set.insert` apply s1 c0
          k1 = map (apply s1) k0
          b1 = apply s1 b
          d1 = d{fr = (l1, EVar evar) : map (second (apply s1)) fs}
      algoMC (d1, b1) k1 c1

algoMC_ (d@ D{fr = (l1, EVar evar) : fs}, b@(BEx (RL l1') bs)) k0 c0
  | l1 == l1' = do
      let (as, is) = getEChoice c0 evar
          -- check which active/inactive labels must be adjusted, depending on b
          blabs = map fst bs
          alabs = Map.keys as
          ilabs = Map.keys is
          i3    = blabs \\  (alabs ++ ilabs) -- new inactive labels
          j1    = alabs \\ blabs -- active labels not in b
      trace ("j1 = " ++ show j1) (return ())
      trace ("i3 = " ++ show i3) (return ())
      if not (null i3 && null j1)
        then do -- adjust labels and run MC again
          -- move the active labels not in b to the inactive ones
          let is' = foldr (\i acc -> Map.insert i
                                        (fromMaybe (error "unexpected")
                                                   (Map.lookup i as)
                                        ) acc
                          ) is j1
          psi3s <- mapM (const (do x <- newPsi
                                   return (SV x))) i3
          let ec   = Map.fromList (zip i3 psi3s) -- add new inactive labels
              is'' = is' `Map.union` ec
              as'  = foldr Map.delete as j1
              c1   = updateEVar evar as' is'' c0
          algoMC (d, b) k0 c1

        else -- run MC on each sub-behaviour of b
          if not (all (`elem` blabs) alabs
                  && all (`elem` alabs ++ ilabs) blabs)
                  then error "malformed external choice"
            else foldM  (\(si, ci) (li, bi)-> do
              let sesi = fromMaybe (
              -- get the session type associated to l_i
                          fromMaybe (error "label undefined") (Map.lookup li as)
                          ) (Map.lookup li is)
                  di = d{fr=(l1, sesi) : map (second (apply si)) fs}
                  bi' = apply si bi
                  ki = map (apply si) k0
              (si', ci') <- algoMC (di, bi') ki ci
              return (si `compose` si', ci')

                        ) (idSub, c0) bs

-- ----------------------------------------------------------------------------
-- Sequencing
-- ----------------------------------------------------------------------------
algoMC_ (d, BSeq b1 b2) k0 c0 = algoMC (d, b1) (b2:k0) c0

-- ----------------------------------------------------------------------------
-- Internal choice
-- ----------------------------------------------------------------------------
algoMC_ (d, BIn b1 b2) k0 c0 = do
  (s1, c1) <- algoMC (d, b1) k0 c0
  let d1  = apply s1 d
      k1  = map (apply s1) k0
      b2' = apply s1 b2
  (s2, c2) <- algoMC (d1, b2') k1 c1
  return (s1 `compose` s2, c2)

-- ----------------------------------------------------------------------------
-- Spawn
-- ----------------------------------------------------------------------------
algoMC_ (d, BSpawn b) k0 c0 = do
  (s1, c1) <- algoMC (emptyDelta , b) [] c0
  let k1  = map (apply s1) k0
      d1  = apply s1 d
  (s2, c2) <- algoMC (d1, BTau) k1 c1
  return (s1 `compose` s2, c2)


-- ----------------------------------------------------------------------------
-- Recursion
-- ----------------------------------------------------------------------------
algoMC_ (d, BRec b _) k0 c0 = do
  -- simplifyBehaviour already takes care of beta vars, so we don't modify c0
  (s1, c1) <- algoMC (emptyDelta , b) [] c0
  let d1  = apply s1 d
      k1  = map (apply s1) k0
  (s2, c2) <- algoMC (d1, BTau) k1 c1
  return (s1 `compose` s2, c2)

-- ----------------------------------------------------------------------------
-- last resort: close the top session and continue with the rest of the stack
-- ----------------------------------------------------------------------------
algoMC_ (d@D {fr = (_, SV psi):_}, b) k c0 = do
  trace "last resort" (return ())
  trace ("k = " ++ show k) (return ())
  let s1 = finalize (freevars $ SV psi)
      d1 = apply s1 d
      b1 = apply s1 b
      k1 = map (apply s1) k
      c1 = apply s1 c0
  (s2, c2) <- algoMC (d1, b1) k1 c1
  return (s1 `compose` s2, c2)

-- TODO: inference fail
algoMC_ (d,b) k c = error $ "Inference failed.\n"
                              ++ showConf (d, b) ++ "\n"
                              ++ "k = " ++ show k ++ "\n"
                              ++ "c = " ++ (render . ppConstraints) c
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

updateEVar :: Psi -> LabelMap -> LabelMap -> Constraints -> Constraints
updateEVar psiint as is = Set.map (\x -> case x of
    EVarC _ _ ivar ->
      if ivar == psiint
        then EVarC as is ivar
        else x
    _ -> x
    )

updateIVar :: Psi -> LabelMap -> Constraints -> Constraints
updateIVar psiint ic = Set.map (\x -> case x of
    IVarC _ ivar ->
      if ivar == psiint
        then IVarC ic ivar
        else x
    _ -> x
    )

getEChoice :: Constraints -> Psi ->  (LabelMap, LabelMap)
getEChoice cs ivar =
      Set.fold  (\x acc -> case x of
                              EVarC as is psi -> if psi == ivar
                                                then (as, is)
                                                else acc
                              _ -> acc)
                (error $ "couln't find evar " ++ show ivar) cs

getIChoice :: Constraints -> Psi ->  LabelMap
getIChoice cs ivar =
      Set.fold  (\x acc -> case x of
                              IVarC ic psi -> if psi == ivar
                                                then ic
                                                else acc
                              _ -> acc)
                (error $ "couln't find ivar " ++ show ivar) cs

substChoiceVars :: Constraints -> (Substitution, Constraints)
substChoiceVars = Set.fold (\c (sub, acc) -> case c of
    EVarC as is (Psi evar) -> let echoice = SExC (Map.toList as) (Map.toList is)
                                  s1      = mkSesSub evar echoice
                              in (sub `compose` s1, acc)
    IVarC is (Psi ivar) -> let ichoice = SInC (Map.toList is)
                               s1      = mkSesSub ivar ichoice
                           in (sub `compose` s1, acc)
    _ -> (sub, c `Set.insert`acc)
    )  (idSub, Set.empty)


algorithmST :: Behav -> Constraints -> IO (Substitution, Constraints)
algorithmST b0 c0 = do
  -- setup and run Algorithm MC
  let b0' = simplifyBehaviour c0 b0  -- remove betas, setup rec
      s1  = simplifyConstraints c0   -- unify \psi vars for all (c ~ \psi) in C
      c1  = apply s1 c0
      b1  = apply s1 b0'
  (s2, c2) <- algoMC_ (D [] Set.empty, b1) [] c1

  -- remove psiint and psiext
  let (s3, c3)  = substChoiceVars c2
      sub       = s1 `compose` s2 `compose` s3
      cs        = apply s3 c3

  return (sub, cs)
