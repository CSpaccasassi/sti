
module Types.TAE where


-------------------------------------------
-- Typing context
-------------------------------------------
-- (type x gammas x Constraints under gammas )   --See Fig. 4.3
--data TypingContext = TCtx [(Type, FreeVars, Constraints )]
--  deriving ( Show )
--  
--instance CanSubstitute TypingContext where
--  apply sigma (TCtx ctx) = TCtx $ map (\(x,y,z) -> let sigma' = deleteGenVars y sigma
--                                                   in ( apply sigma' x, y, apply sigma' z ) ) ctx
--  freevars (TCtx ctx) = foldl (\c (t, a, z) -> c `unionFV` ((freevars t `unionFV` freevars z) `diffFV` a)) emptyFreeVars ctx
--  chanvars (TCtx ctx) = foldl (\c (t, a, z) -> c `unionFV` chanvars t `unionFV` freevars z `diffFV` a) emptyFreeVars ctx
--  
--  
--singletonTCtx :: Type -> (Type, FreeVars, Constraints)
--singletonTCtx t = ( t, emptyFreeVars, Set.empty )







 


--instance CanSubstitute Behaviour where
--  apply sigma (BVar i)          = BVar ( fromMaybe i ( Map.lookup i ( bsub sigma ) ) )
--  apply _     BNil              = BNil
--  apply sigma (BSeq b1 b2)      = BSeq     (apply sigma b1) (apply sigma b2)
--  apply sigma (BPlus b1 b2)     = BPlus    (apply sigma b1) (apply sigma b2)
--  apply sigma (BPar b1 b2)      = BPar     (apply sigma b1) (apply sigma b2)
--  apply sigma (BNewChan t r)    = BNewChan (apply sigma t)  (apply sigma r)
--  apply sigma (BSend r t)       = BSend    (apply sigma r)  (apply sigma t)  
--  apply sigma (BRecv r t)       = BRecv    (apply sigma r)  (apply sigma t)
--  apply _     BTau              = BTau 
--  
--  freevars (BVar i)         = FreeVars Set.empty ( Set.singleton i ) Set.empty 
--  freevars BNil             = emptyFreeVars
--  freevars (BSeq b1 b2)     = freevars b1 `unionFV` freevars b2
--  freevars (BPlus b1 b2)    = freevars b1 `unionFV` freevars b2
--  freevars (BPar b1 b2)     = freevars b1 `unionFV` freevars b2
--  freevars (BNewChan t r)   = freevars t `unionFV` freevars r
--  freevars (BSend r t)      = freevars r `unionFV` freevars t  
--  freevars (BRecv r t)      = freevars r `unionFV` freevars t
--  freevars BTau             = emptyFreeVars
--
--  chanvars (BVar _)         = emptyFreeVars 
--  chanvars BNil             = emptyFreeVars
--  chanvars (BSeq b1 b2)     = chanvars b1 `unionFV` chanvars b2
--  chanvars (BPlus b1 b2)    = chanvars b1 `unionFV` chanvars b2
--  chanvars (BPar b1 b2)     = chanvars b1 `unionFV` chanvars b2
--  chanvars (BNewChan t r)   = freevars t  `unionFV` freevars r
--  chanvars (BSend r t)      = freevars r  `unionFV` freevars t  
--  chanvars (BRecv r t)      = freevars r  `unionFV` freevars t
--  chanvars BTau             = emptyFreeVars
---- creates a fresh behaviour variable
--newBVar :: IO Behaviour
--newBVar = do
--  freshId <- newUnique
--  return (BVar freshId)












  
-------------------------------------------
-- Substitution
-------------------------------------------
--
--
--showTSub :: TSub -> String
--showTSub s = "[" ++ concatMap ( \(x,y) -> "-(a" ++ show x ++ ", " ++ show y ++ ")-") (Map.toList s) ++ "]"
--
--showSub :: String -> BSub -> String
--showSub c s = concatMap ( \(x,y) -> "(" ++ c ++ show x ++ ", " ++ show y) (Map.toList s)
--
--
--instance Show Unique where
--  show i = show $ hashUnique i
--  
--instance Show Substitution where
--  show ( Sub ts bs rs ) = "[" ++ intercalate "," [showTs, showBs, showRs] ++ "]"
--    where showTs = if Map.null ts 
--                    then ""
--                    else "TSub: " ++ showTSub ts 
--          showBs = if Map.null bs
--                    then ""
--                    else "BSub: " ++ showSub "b" bs ++ ", " 
--          showRs = if Map.null rs
--                    then ""
--                    else "RSub: " ++ showSub "c" rs 
--
--
--
--
--instance Show FreeVars where
--  show ( FreeVars ts bs rs ) = "FV: " ++ tv ++ bv ++ rv 
--    where tv = if Set.null ts
--                then ""
--                else "TVars: [" ++ intercalate "," (map (\ x -> 'a' : show x) (Set.toList ts)) ++ "] "
--          bv = if Set.null bs
--                then ""
--                else "BVars: [" ++ intercalate "," (map (\ x -> 'b' : show x) (Set.toList bs)) ++ "] "
--          rv = if Set.null rs
--                then ""
--                else "RVars: [" ++ intercalate "," (map (\ x -> 'r' : show x) (Set.toList rs)) ++ "]"
--
--
--fvMember :: Unique -> FreeVars -> Bool
--fvMember i ( FreeVars t b r ) = i `Set.member` ( t `Set.union` b `Set.union` r ) 
--
--
--
--idSub :: Substitution
--idSub = Sub m m m
--  where m = Map.empty
--
--
--mkTSub :: Unique -> Unique -> Substitution
--mkTSub i1 i2 = Sub m' m m
--  where m' = Map.fromList [(i1, TVar i2)] 
--        m  = Map.empty
--        
--        
--mkBSub :: Unique -> Unique -> Substitution
--mkBSub i1 i2 = Sub m m' m
--  where m' = Map.fromList [(i1, i2)] 
--        m  = Map.empty
--
--
--mkRSub :: Unique -> Unique -> Substitution
--mkRSub i1 i2 = Sub m m m'
--  where m' = Map.fromList [(i1, i2)] 
--        m  = Map.empty
--
--
--compose :: Substitution -> Substitution -> Substitution 
--compose s1 s2 = Sub ts bs rs
--  where ts = combineType (tsub s1) (tsub s2)
--        bs = combine (bsub s1) (bsub s2)
--        rs = combine (rsub s1) (rsub s2)
--        combine r1 r2 = Map.map (\a -> fromMaybe a (Map.lookup a r2)) r1 `Map.union` Map.difference r2 r1
--        combineType t1 t2 = Map.map (apply s2) t1 `Map.union` Map.difference t2 t1
--
--
--deleteGenVars :: FreeVars -> Substitution -> Substitution
--deleteGenVars (FreeVars ts' bs' rs' ) (Sub ts bs rs) = Sub ts'' bs'' rs'' 
--  where ts'' = Set.fold Map.delete ts ts' 
--        bs'' = Set.fold Map.delete bs bs'
--        rs'' = Set.fold Map.delete rs rs'
--          
--          
--emptyFreeVars :: FreeVars
--emptyFreeVars = FreeVars Set.empty Set.empty Set.empty
--
--
--unionFV :: FreeVars -> FreeVars -> FreeVars
--unionFV ( FreeVars t1 b1 r1 ) ( FreeVars t2 b2 r2 ) =   
--                                                  FreeVars ( t1 `Set.union` t2 ) 
--                                                           ( b1 `Set.union` b2 ) 
--                                                           ( r1 `Set.union` r2 )
--
--diffFV :: FreeVars -> FreeVars -> FreeVars
--diffFV ( FreeVars t1 b1 r1 ) ( FreeVars t2 b2 r2 ) = 
--                                            FreeVars ( t1 `Set.difference` t2 )
--                                                     ( b1 `Set.difference` b2 )
--                                                     ( r1 `Set.difference` r2 )
--
--intersectionFV :: FreeVars -> FreeVars -> FreeVars
--intersectionFV ( FreeVars t1 b1 r1 ) ( FreeVars t2 b2 r2 ) = 
--                                          FreeVars ( t1 `Set.intersection` t2 ) 
--                                                   ( b1 `Set.intersection` b2 ) 
--                                                   ( r1 `Set.intersection` r2 )
--
--mkFV ::[Unique] -> [Unique] -> [Unique] -> FreeVars
--mkFV ts bs rs = FreeVars ( Set.fromList ts ) ( Set.fromList bs ) ( Set.fromList rs )
--
--mkTFV ::[Unique] -> FreeVars
--mkTFV ts = FreeVars ( Set.fromList ts ) Set.empty Set.empty 
--
--mkBFV ::[Unique] -> FreeVars
--mkBFV bs = FreeVars Set.empty ( Set.fromList bs ) Set.empty 
--
--mkRFV ::[Unique] -> FreeVars
--mkRFV rs = FreeVars Set.empty Set.empty ( Set.fromList rs )
--
--toListFV :: FreeVars -> [Unique]
--toListFV ( FreeVars ts bs rs ) = Set.toList ts ++ Set.toList bs ++ Set.toList rs 
--
--
--sizeFV :: FreeVars -> Int
--sizeFV ( FreeVars ts bs rs ) = Set.size ts + Set.size bs + Set.size rs




----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------





--typechanVars :: Type -> Set.Set Unique
--typechanVars t = tvars fvT `Set.union` bvars fvT `Set.union` rvars fvT 
--  where fvT       = freevars t
--
--
--chanVars :: Type -> Region -> Set.Set Unique
--chanVars t r = tvars fvT `Set.union` bvars fvT `Set.union` rvars fvT `Set.union` rvars fvR 
--  where fvT       = freevars t
--        fvR       = freevars r

-------------------------------------------
-- Monotonicity
-------------------------------------------

--class Monotonic a where
--  isMonotonic     :: Unique -> a -> Bool
--  isAntiMonotonic :: Unique -> a -> Bool
--  
--
--
--instance Monotonic Type where
--  isMonotonic _ (TVar _)          = True
--  isMonotonic _ TUnit             = True
--  isMonotonic _ TBool             = True
--  isMonotonic _ TInt              = True
--  isMonotonic i (TTup t1 t2)      = isMonotonic i t1 && isMonotonic i  t2
--  isMonotonic i (TArrowB t1 _ t2) = isAntiMonotonic i t1 && isMonotonic i t2
--  isMonotonic i (TChan t _)       = i `Set.notMember` typechanVars t
--       
--  isAntiMonotonic  i (TVar j)          = i /= j
--  isAntiMonotonic  _ TUnit             = True
--  isAntiMonotonic  _ TBool             = True
--  isAntiMonotonic  _ TInt              = True
--  isAntiMonotonic  i (TTup t1 t2)      = isAntiMonotonic i t1 && isAntiMonotonic  i  t2
--  isAntiMonotonic  i (TArrowB t1 b t2) = isNotBeta && isMonotonic i t1 && isAntiMonotonic i t2
--    where isNotBeta = case b of 
--                  BVar j  -> i /= j
--                  _       -> True
--  isAntiMonotonic i (TChan t r)       = i `Set.notMember` chanVars t r
--
--
--instance Monotonic Behaviour where
--  isMonotonic _ (BVar _)         = True 
--  isMonotonic _ BNil             = True
--  isMonotonic i (BSeq b1 b2)     = isMonotonic i b1 && isMonotonic i b2
--  isMonotonic i (BPlus b1 b2)    = isMonotonic i b1 && isMonotonic i b2
--  isMonotonic i (BPar b1 b2)     = isMonotonic i b1 && isMonotonic i b2
--  isMonotonic i (BNewChan t r)   = i `Set.notMember` chanVars t r
--  isMonotonic i (BSend r t)      = i `Set.notMember` chanVars t r 
--  isMonotonic i (BRecv r t)      = i `Set.notMember` chanVars t r
--  isMonotonic _ BTau             = True
--  
--  isAntiMonotonic i (BVar j)         = i /= j 
--  isAntiMonotonic _ BNil             = True
--  isAntiMonotonic i (BSeq b1 b2)     = isAntiMonotonic i b1 && isAntiMonotonic i b2
--  isAntiMonotonic i (BPlus b1 b2)    = isAntiMonotonic i b1 && isAntiMonotonic i b2
--  isAntiMonotonic i (BPar b1 b2)     = isAntiMonotonic i b1 && isAntiMonotonic i b2
--  isAntiMonotonic i (BNewChan t r)   = i `Set.notMember` chanVars t r
--  isAntiMonotonic i (BSend r t)      = i `Set.notMember` chanVars t r 
--  isAntiMonotonic i (BRecv r t)      = i `Set.notMember` chanVars t r
--  isAntiMonotonic _ BTau             = True
--
--
--instance Monotonic Region where
--  isMonotonic _ (RVar _)    = True
--  isMonotonic _ (RLabel _)  = True
--  isMonotonic _ (RRLabel _) = True
--
--  isAntiMonotonic i (RVar j)    = i /= j
--  isAntiMonotonic _ (RLabel _)  = True 
--  isAntiMonotonic _ (RRLabel _) = True
--  
---- variables are omitted according to the definition of LHS and Figure 4.8  
--instance Monotonic Constraint where               
--  isMonotonic i ( TC t1 _ ) = isMonotonic i t1
--  isMonotonic i ( BC b1 _ ) = isMonotonic i b1
--  isMonotonic i ( RC r1 _ ) = isMonotonic i r1
--  isMonotonic i ( NTC t1 _ ) = isMonotonic i t1
----  isMonotonic i ( NTC2 t1 _ ) = isMonotonic i t1 
--   
--  
--  isAntiMonotonic i ( TC t1 _ ) = isAntiMonotonic i t1
--  isAntiMonotonic i ( BC b1 _ ) = isAntiMonotonic i b1
--  isAntiMonotonic i ( RC r1 _ ) = isAntiMonotonic i r1
--  isAntiMonotonic i ( NTC t1 _ ) = isAntiMonotonic i t1
--  isAntiMonotonic i ( NTC2 t1 _ ) = isAntiMonotonic i t1
  
--instance Monotonic Constraints where
--  isMonotonic i cs     = all (isMonotonic i)      ( Set.toList cs )
--  isAntiMonotonic i cs = all (isAntiMonotonic i)  ( Set.toList cs )