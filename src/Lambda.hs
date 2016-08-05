module Lambda (doLambdaStep) where

import Syntax

-------------------
-- Lambda Steps
-------------------
-- takes a lambda step,  or returns Nothing. 
reductionStep ::  TProcess -> Maybe Expression
reductionStep p =  case expr p of 
  (V v)                           -> Just $ V v
  (App v@(V (Fun _ x _ e1)) e2)   -> Just (Let x e2 e1')
                                    -- shift the index of "x" by one to form a new LET expression
                                    -- and substitute "f" in the function body with the function itself
                                    where 
                                      e1'  = shift 1 e1''
                                      e1'' = subst 0 v e1
  (Let _ v@(V _) e2)              -> Just (subst 0 v e2)
  (If (V (B b))  e2 e3)           -> Just (if b then e2 else e3)
  (Op op (V (N n1)) (V (N n2)))   -> Just (V x)
                                      where 
                                        x = case op of
                                          "+"   -> N $ n1 + n2
                                          "-"   -> N $ n1 - n2
                                          "*"   -> N $ n1 * n2
                                          "<="  -> B $ n1 <= n2
                                          "=="  -> B $ n1 == n2
                                          _     -> error $ "undefined op: " ++ show op
  (Op op (V (Chan c1 _)) (V (Chan c2 _))) -> Just (V x)
                                      where 
                                        x = case op of
                                          "=="  -> B $ c1 == c2
                                          _     -> error $ "undefined op: " ++ show op
  
  (Fst (V (VTup v1 _)))         -> Just (V v1)
  (Snd (V (VTup _ v2)))         -> Just (V v2)
  
  (SignalSuccess)               -> Just SignalSuccess
  
  _                             -> Nothing


-- splits an expression into inner expression within a context, and the context itself
popCtx :: Expression -> (Expression, ContextHole)
popCtx (V v)                  = (V v,   \_        -> V v)

popCtx (App e1 e2)            = (e1,                 (`App` e2))
popCtx (Let x e1 e2)          = (e1,    \e        -> Let x e e2)
popCtx (If e1 e2 e3)          = (e1,    \e        -> If e e2 e3)
popCtx (Op op (V (N n1)) e2)  = (e2,                 Op op (V (N n1)))
popCtx (Op op e1 e2)          = (e1,    \e        -> Op op e e2)

popCtx (Tup (V v1) e2)        = (e2,    \e        -> case e of 
                                                        V v2 -> V $ VTup v1 v2
                                                        e'   -> error $ "unexpected expression (" ++ show e' ++ ") while executing pair " ++ show  (Tup (V v1) e2))
popCtx (Tup e1 e2)            = (e1,                 (`Tup` e2))
popCtx (Fst e1)               = (e1,                 Fst)
popCtx (Snd e1)               = (e1,                 Snd)

popCtx e                      = error $ "unexpected context: " ++ pp e


-- returns either an expression after a lambda step or an inner expression within a context
reduce :: TProcess -> Either Expression (Expression, ContextHole)
reduce p = case reductionStep p of
  Just x  -> Left x
  Nothing -> Right (popCtx (expr p))


-- reduce by one step or pop a context
doLambdaStep :: TProcess -> TProcess
doLambdaStep p = 
        case reduce p of
            -- Left: perform lambda steps until the context stack is empty
            Left e             -> p {expr = e}
            
            -- Right: add popped context on the stack
            Right (e, c')     -> p {expr = e, ctx = c':pCtx}
          where pCtx = ctx p