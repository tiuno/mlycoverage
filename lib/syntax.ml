type variable = string

and program = definition list

and definition =
  | Def of variable * expression
  | RecValues of (variable * expression) list

and expression =
  | Var of variable
  | Fun of variable * expression
  | App of expression * expression
  | Fby of expression * expression
  | At of expression * expression
  | Tuple of expression list
  | MuTuple of (variable * expression) list
  | Primitive of primitive
  | Const of const
  | TypedExp of typed_expression

and const =
  | Unit
  | Nil
  | Bool of bool
  | Int  of int

and primitive =
  | Input      of string
  | NegB       of expression
  | Proj       of int * int * expression
  | Plus       of expression * expression
  | Minus      of expression * expression
  | Times      of expression * expression
  | Div        of expression * expression
  | Eq         of expression * expression
  | Neq        of expression * expression
  | AndB       of expression * expression
  | OrB        of expression * expression
  | Cons       of expression * expression
  | Map        of expression * expression
  | Iter       of expression * expression
  | FoldL      of expression * expression * expression
  | FoldR      of expression * expression * expression
  | IfThenElse of expression * expression * expression

and typed_expression = expression * nominal_type

and tyvar = int

and 'a typ =
  | TyUnit
  | TyInt
  | TyBool
  | TyList of 'a typ
  | TyVar of 'a
  | TyArrow of 'a typ * 'a typ
  | TyStream of 'a typ
  | TyProduct of 'a typ list

and nominal_type = tyvar typ
