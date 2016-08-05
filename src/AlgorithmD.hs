
module AlgorithmD where

import Types

import qualified Data.Set as DS
import qualified Data.Map as DM

breakDual :: Constraint -> Maybe (Substitution, Constraints)
breakDual c = case c of
  -- ended sessions
  DC SEnd SEnd -> Just (idSub, DS.empty)

  -- send/receive
  DC (SSend t1 ses1) (SRecv t2 ses2) -> doSendRcv t1 ses1 t2 ses2
  DC (SRecv t2 ses2) (SSend t1 ses1) -> doSendRcv t2 ses2 t1 ses1

  -- delegate/resume
  DC (SDeleg d ses1) (SResume r ses2) -> doDelegRes d r  ses1 ses2
  DC (SResume r ses2) (SDeleg d ses1) -> doDelegRes r d  ses2 ses1

  -- external/internal choice
  DC (SExC es _) (SInC is) -> doInExChoice is es
  DC (SInC is) (SExC es _) -> doInExChoice is es

  _ -> Nothing
 where  doSendRcv t1 s1 t2 s2 = Just (idSub, DS.fromList [DC s1 s2, TC t1 t2])
        doDelegRes d r ses1 ses2 =
          let (s1, c1) = sesSubtype  d r
              ses1' = apply s1 ses1
              ses2' = apply s1 ses2
          in Just (s1, DC ses1' ses2' `DS.insert` c1)
        doInExChoice is es =
          let c1 = foldr (\(l, ses1) acc
                    ->let c' = foldr (\(l', ses2) acc'
                                  -> if l == l'
                                      then DC ses1 ses2
                                      else acc'
                                  ) (error "D: missing label") es
                      in c' `DS.insert` acc
                    ) DS.empty is
          in Just (idSub, c1)



algoD :: Ses -> Ses -> (Substitution, Constraints)
algoD SEnd SEnd = (idSub, DS.empty)
-- algoD (SV (Psi psi)) ses2 = (mkSesSub psi ses2, DS.empty)
-- algoD ses1 (SV (Psi psi)) = (mkSesSub psi ses1, DS.empty)

algoD (SSend t1 ses1) (SRecv t2 ses2) = let (s1, c1) = algoD ses1 ses2
                                        in (s1, DS.insert (DC ses1 ses2)
                                               $ DS.insert (TC t1 t2) c1)

algoD (SRecv t1 ses1) (SSend t2 ses2) = let (s1, c1) = algoD ses1 ses2
                                        in (s1, DS.insert (TC t2 t1) c1)

algoD _ _       = error "algoD failed"

doAlgoD_ :: [Constraint] -> Maybe (Substitution, Constraints)
doAlgoD_ []     = Nothing
doAlgoD_ (c:c0) = case breakDual c of
                  Nothing -> case doAlgoD_ c0 of
                      Nothing -> Nothing
                      Just (s1, c1)  -> Just (s1, apply s1 c `DS.insert` c1)
                  Just (s1, c1) -> let c2 = c1 `DS.union` apply s1 (DS.fromList c0)
                                   in Just (s1, c2)

doAlgoD :: Constraints -> (Substitution, Constraints)
doAlgoD c0 =  let c1 = DS.toList c0
              in case doAlgoD_ c1 of
                          Nothing -> (idSub, c0)
                          Just (s2, c2)  -> let (s3, c3) = doAlgoD c2
                                            in (s2 `compose` s3, c3)

algorithmD :: Constraints -> (Substitution, Constraints)
algorithmD  cs =
  -- create duality constraints \eta\bowtie\eta'
  -- for each pair c ~\eta, \bar c ~ \eta' in C
  let (c1, _) = DS.fold (\co acc ->
                    case co of
                        SC ses (SReq c _) -> doLookup acc ses (c, True)
                        SC ses (SAcc c _) -> doLookup acc ses (c, False)
                        _  -> acc
                    ) (DS.empty, DM.empty) cs
      (s2, c2) = doAlgoD c1 -- (s1, apply s1 cs `DS.union` c1)
  in (s2, apply s2 cs `DS.union` c2)
 where
   doLookup acc@(c0, m0) ses (c, b) =
      case DM.lookup c m0 of
          Nothing   -> let m1 = DM.insert c (ses, b) m0
                       in (c0, m1)
          Just (ses', b') -> if b == b'
                              then acc
                              else (DC ses ses' `DS.insert` c0, m0)
