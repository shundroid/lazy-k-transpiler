module Type where
data Ast =
  Var String |
  App Ast Ast |
  Lam String Ast |
  S | K | I
  deriving (Show, Eq)
