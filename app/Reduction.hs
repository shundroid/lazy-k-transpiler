module Reduction where
import Control.Monad.State (State, MonadState (get), modify, runState, evalState)
import Type (Ast (..))

type ReductionState = State Int
newvar :: ReductionState String
newvar = do
  i <- get
  modify (+1)
  return $ "t" ++ show i

reductable :: Ast -> Bool
reductable (App (Lam _ _) _) = True
reductable (App S _) = True
reductable (App K _) = True
reductable (App I _) = True
reductable (App x y) = reductable x || reductable y
reductable (Lam _ e) = reductable e
reductable _ = False

reduction :: Ast -> ReductionState Ast
reduction ast = case ast of
  App (Lam x e) y -> do
    e1 <- substitute e x y
    reduction e1
    -- if reductable e1 then reduction e1 else return e1
  App e y -> do
    e1 <- reduction e
    case e1 of
      Lam _ _ -> reduction (App e1 y)
      _ -> do
        y1 <- reduction y
        return $ App e1 y1
  Lam x (App y (Var z)) | x == z -> return y
  Lam x e -> do
    e1 <- reduction e
    return $ Lam x e1
  S -> do
    t1 <- newvar
    t2 <- newvar
    t3 <- newvar
    return $ Lam t1 (Lam t2 (Lam t3 (App (App (Var t1) (Var t3)) (App (Var t2) (Var t3)))))
  K -> do
    t1 <- newvar
    t2 <- newvar
    return $ Lam t1 (Lam t2 (Var t1))
  I -> do
    t1 <- newvar
    return $ Lam t1 (Var t1)
  x -> return x
  where
    freevars :: Ast -> [String]
    freevars e = case e of
      Var x -> [x]
      App e1 e2 -> freevars e1 ++ freevars e2
      Lam x e -> filter (/= x) $ freevars e
      _ -> [] -- including SKI
    alpha :: Ast -> String -> String -> Ast
    alpha e x newX = case e of
      Var y -> if x == y then Var newX else Var y
      App e1 e2 -> App (alpha e1 x newX) (alpha e2 x newX)
      Lam y e -> if x == y then Lam y e else Lam y (alpha e x newX)
      _ -> e -- including SKI
    replace :: Ast -> String -> Ast -> [String] -> ReductionState Ast
    replace e x newE fvs = case e of
      Var y -> if x == y then return newE else return $ Var y
      App e1 e2 -> do
        newE1 <- replace e1 x newE fvs
        newE2 <- replace e2 x newE fvs
        return $ App newE1 newE2
      Lam y e ->
        if x == y then return $ Lam y e
        else if y `elem` fvs then do
          t <- newvar
          Lam t <$> replace (alpha e y t) x newE fvs
        else Lam y <$> replace e x newE fvs
      _ -> return e -- including SKI
    substitute :: Ast -> String -> Ast -> ReductionState Ast
    substitute e x e2 =
      let fvs = freevars e2 in
      replace e x e2 fvs

runReduction :: Ast -> Ast
runReduction x = evalState (reduction x) 0