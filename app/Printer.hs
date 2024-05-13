module Printer where
import Type (Ast (..))
import Data.Text

cc :: Ast -> Text
cc S = pack "S"
cc K = pack "K"
cc I = pack "I"
cc (App x (App y z)) =
  cc x <>
  pack "(" <>
  cc (App y z) <>
  pack ")"
cc (App x y) = cc x <> cc y

unlambda :: Ast -> Text
unlambda S = pack "s"
unlambda K = pack "k"
unlambda I = pack "i"
unlambda (App x y) = pack "`" <> unlambda x <> unlambda y
