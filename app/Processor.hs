module Processor where
import Type
import Debug.Trace (trace)

checkFreeOccurrence :: String -> Ast -> Bool
checkFreeOccurrence x y = case y of
  Var y -> x == y
  App y1 y2 -> checkFreeOccurrence x y1 || checkFreeOccurrence x y2
  Lam y1 y2 -> x /= y1 && checkFreeOccurrence x y2
  S -> False
  K -> False
  I -> False

toSKI :: Ast -> Ast
toSKI x = case x of
  Var x -> Var x
  App x y -> App (toSKI x) (toSKI y)
  Lam x e | not $ checkFreeOccurrence x e ->
    App K (toSKI e)
  Lam x (Var y) | x == y -> I
  Lam x (Lam y e) -> toSKI (Lam x $ toSKI $ Lam y e)
  Lam x (App e1 e2) ->
    App (App S $ toSKI (Lam x e1)) (toSKI (Lam x e2))
  Lam x (App e (Var y)) | x == y -> toSKI e
  S -> S
  K -> K
  I -> I
  _ -> trace ("toSKI: " ++ show x) undefined

combB = App (App S (App K S)) K

simplify :: Ast -> Ast
simplify x = case x of
  App I x -> simplify x
  App (App S K) _ -> I
  App (App S x) (App K I) | x == combB -> I
  App (App S K) (App K I) -> I
  App (App K K) I -> K
  App (App (App S K) S) K -> K
  App (App K K) (App S K) -> K
  App (App S (App K x)) I -> simplify x
  -- App (App S (App K S)) K -> combB
  -- App (App S x) y 
  App x y -> App (simplify x) (simplify y)
  S -> S
  K -> K
  I -> I

iterateSimplify :: Ast -> Ast
iterateSimplify x = let y = simplify x in
  if x == y then x else iterateSimplify y
