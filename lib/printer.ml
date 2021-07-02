open Syntax
open PPrint

let rec print_type x = print_type_arrow x

and print_type_arrow t =
  match t with
  | TyArrow (t1, t2) ->
     infix 2 0 !^" -> " (print_type t1) (print_type_atom t2)
  | TyList t ->
     prefix 2 1 (print_type_atom t) !^"list"
  | TyStream t ->
     prefix 2 1 (print_type_atom t) !^"stream"
  | _ ->
     print_type_atom t

and print_type_atom t =
  match t with
  | TyUnit ->
     !^"unit"
  | TyInt ->
     !^"int"
  | TyBool ->
     !^"bool"
  | TyVar tv ->
      OCaml.int tv
  | TyProduct ts (* (t1, t2) *) ->
     surround 2 0 lparen (separate_map star print_type ts) rparen
  | TyArrow _ | TyList _ | TyStream _ ->
     group (parens (print_type t))

let print_typed_expression print_expression e t =
  let typ = print_type t in
  let exp = print_expression e in
  exp ^^ colon ^^ typ


let print_tuple print_component components =
  let contents = match components with
    | [component] ->
       print_component component ^^ comma
    | _ ->
       separate_map (comma ^^ space) print_component components in
  surround 2 0 lparen contents rparen

let print_term_list print_term list =
  match list with
  | Some xs, _ ->
     let contents = match xs with
       | [component] ->
          print_term component
       | _ ->
          separate_map (semi ^^ space) print_term xs
     in
     surround 2 0 lbracket contents rbracket
  | _, Some xs ->
     print_term xs
  | _ -> assert false

let print_term_stream print_component components =
  separate_map (twice colon) print_component components

let print_term_binding print_term (x,t) =
   !^x ^^ !^" =" ^^ jump 1 1 (print_term t)

let print_term_mutuple print_term ts =
  let contents = match ts with
    | [eq] ->
       print_term_binding print_term eq
    | _ ->
       separate_map (hardline ^^ !^"and ") (print_term_binding print_term) ts
  in
  !^"rectup " ^^ contents

let rec print_term t = print_term_abs t

and print_term_abs t =
  match t with
  | Fun (x, t) ->
     !^"fun " ^^ !^x ^^ !^" -> " ^^ print_term_abs t
  | MuTuple ts ->
       print_term_mutuple print_term_abs ts
  | t ->
     print_term_app t

and print_term_app t =
  match t with
  | App (t1, t2) ->
     print_term_app t1 ^^ space ^^ print_term_atom t2
  | At (s, t) ->
     print_term_infix_app at s t
  | Primitive p ->
     print_term_primitive p
  | t ->
     print_term_atom t

and print_list x =
  let rec walk_list x =
    match x with
    | Primitive (Cons (x, y)) ->
       begin match walk_list y with
       | Some xs, _ ->
          (Some (x::xs), None)
       | (_, Some xs) ->
          (None, Some (App(App(Var "cons", x), xs)))
       | _ -> assert false
       end
    | Const Nil -> (Some [],None)
    | x -> (None, Some x)
  in
  print_term_list print_term (walk_list x)
and print_term_atom t =
  match t with
  | Var x ->
     !^x
  | Const c ->
     print_term_constant c
  | Tuple t ->
     print_tuple print_term t
  | Primitive Cons _ as xs ->
     print_list xs
  | Fby _ ->
     let rec walk_stream exp =
       match exp with
       | Fby (a, b) ->
          a::walk_stream b
       | _ ->
          [exp]
     in
     print_term_stream print_term (walk_stream t)
  | TypedExp (e, t) ->
     print_typed_expression print_term e t
  | App _ | Fun _ | MuTuple _ | Primitive _ | At _ ->
     group (parens (print_term t))

and print_term_constant c =
  match c with
  | Unit -> parens empty
  | Int i -> OCaml.int i
  | Bool b -> OCaml.bool b
  | Nil -> brackets empty

and print_term_infix_app op t1 t2 =
  group (infix 1 1 op (print_term_atom t1) (print_term_atom t2))

and print_term_primitive p =
  match p with
  | Input l ->
     prefix 1 1 !^"input" (surround 1 0 dquote !^l dquote)
  | Proj (i, m, MuTuple ts) ->
       prefix 1 1 (!^"proj " ^^ OCaml.int i ^^ space ^^ OCaml.int m)
         (print_term_atom (MuTuple ts))
  | Proj (i, m, t) ->
     prefix 1 1 (!^"proj " ^^ OCaml.int i ^^ space ^^ OCaml.int m)
       (print_term_atom t)
  | NegB t ->
     prefix 1 1 !^"not" (print_term_atom t)
  | Plus (x, y) ->
     print_term_infix_app plus x y
  | Minus (x, y) ->
     print_term_infix_app minus x y
  | Times (x, y) ->
     print_term_infix_app star x y
  | Div (x, y) ->
     print_term_infix_app slash x y
  | Eq (x, y) ->
     print_term_infix_app equals x y
  | Neq (x, y) ->
     print_term_infix_app !^"<>" x y
  | AndB (x, y) ->
     print_term_infix_app (twice ampersand) x y
  | OrB (x, y) ->
     print_term_infix_app (twice bar) x y
  | Map (x, y) ->
     prefix 1 1 !^"map" (print_term_atom x) ^^ jump 1 1 (print_term_atom y)
  | FoldL (x, y, z) ->
     prefix 1 1 !^"fold_left" (print_term_atom x) ^^
       jump 1 1 (print_term_atom y) ^^ jump 1 1 (print_term_atom z)
  | FoldR (x, y, z) ->
     prefix 1 1 !^"fold_right" (print_term_atom x) ^^
       jump 1 1 (print_term_atom y) ^^ jump 1 1 (print_term_atom z)
  | Iter (x, y) ->
     prefix 1 1 !^"iter" (print_term_atom x) ^^ jump 1 1 (print_term_atom y)
  | IfThenElse (c, x, y) ->
     prefix 1 1 !^"if" (print_term_atom c) ^^
       jump 0 1 !^"then" ^^ jump 1 1 (print_term_atom x) ^^
         jump 0 1 !^"else" ^^ jump 1 1 (print_term_atom y)
  | _ ->
     print_term_atom (Primitive p)

let document_to_string print_document doc =
  let buffer = Buffer.create 256
  and ppv = print_document doc in
  PPrint.ToBuffer.pretty 0.9 80 buffer ppv;
  Buffer.contents buffer

let string_of_term t =
  document_to_string print_term t

let string_of_type t =
  document_to_string print_type t

let document_to_format print_document fmt doc =
  let doc = print_document doc in
  PPrint.ToFormatter.pretty 0.9 80 fmt (group doc)

let fmt_of_term fmt t =
  document_to_format print_term fmt t

let fmt_of_type fmt t =
  document_to_format print_type fmt t
