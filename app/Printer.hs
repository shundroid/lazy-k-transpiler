module Printer where
import Type (Ast (..))
import Data.Text

cc :: Ast -> Text
cc S = pack "S"
cc K = pack "K"
cc I = pack "I"
cc (App (App x y) z) =
  pack "(" <>
  cc x <>
  cc y <>
  pack ")" <>
  cc z
cc (App x y) = cc x <> cc y

unlambda :: Ast -> Text
unlambda S = pack "s"
unlambda K = pack "k"
unlambda I = pack "i"
unlambda (App x y) = pack "`" <> unlambda x <> unlambda y
