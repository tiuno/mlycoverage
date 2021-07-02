%{ (* Emacs, open this with -*- tuareg -*- *)
   open Syntax

   type binop =
     | PPlus
     | PMinus
     | PTimes
     | PDiv
     | PEq
     | PNeq
     | PAndB
     | POrB

   let make_binop lhs op rhs =
     match op with
     | PPlus  ->
        Plus (lhs, rhs)
     | PMinus ->
        Minus (lhs, rhs)
     | PTimes ->
        Times (lhs, rhs)
     | PDiv ->
        Div (lhs, rhs)
     | PEq ->
        Eq (lhs, rhs)
     | PNeq ->
        Neq (lhs, rhs)
     | PAndB ->
        AndB (lhs, rhs)
     | POrB ->
        OrB (lhs, rhs)

   let tuple_or_term = function
     |[] ->
       Const Unit
     |[x] ->
       x
     | _::_ as ts->
        Tuple ts

   let make_list xs =
     match xs with
     | [] -> Const Nil
     | _ ->
        List.fold_right (fun x acc -> Primitive (Cons (x, acc))) xs (Const Nil)

	%}

%token EOF
%token <int> INT
%token MUTUP
%token AND
%token LET
%token FUN
%token NOT
%token IF
%token THEN
%token ELSE
%token EQUAL
%token NEQUAL
%token PROJ
%token FBY
%token AT
%token ARROW
%token COMMA
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token MULT
%token DIV
%token LAND
%token LOR
%token INPUT
%token NIL
%token MAP
%token ITER
%token FOLD_LEFT
%token FOLD_RIGHT
%token CONS
%token SEMI_COLON
%token DOUBLE_QUOTE
%token LBRACKET
%token RBRACKET
%token <bool> TRUE
%token <bool> FALSE
%token <string> ID

%right FBY
%right ARROW
%nonassoc ELSE
%nonassoc EQUAL NEQUAL LAND LOR
%left PLUS MINUS
%left MULT DIV

%start <Syntax.program> program
%start <Syntax.expression> exp
%%

exp:
  | a=expression EOF { a }
  | error
    {
      failwith "error parsing expression"
    }

program:
  | ds=binding* EOF { ds }
  | error
    {
      failwith "error parsing definition"
    }

binding:
  | LET d=definition
    {
      let (i, t) = d in Def (i, t)
    }
  | xs=mutuple
    {
      RecValues xs
    }

expression:
  | a=term { a }
  | xs=mutuple { MuTuple xs }

%inline mutuple:
  | MUTUP xs=separated_nonempty_list (AND, definition) { xs }

%inline definition:
  | i=ID EQUAL t=term { (i, t) }

term:
  | t=abstraction
    {
      t
    }
  | IF c=term THEN x=term ELSE y=term
    {
      Primitive (IfThenElse (c, x, y))
    }
  | t=term FBY u=term
    {
      Fby(t, u)
    }
  | lhs=term op=binop rhs=term
    {
      Primitive (make_binop lhs op rhs)
    }
  | t=simple_term
    {
      t
    }

%inline abstraction:
  | FUN i=ID ARROW t=term { Fun (i, t) }

simple_term:
  | t=simple_term u=atomic_term
    {
      App (t, u)
    }
  | NOT x=atomic_term
    {
      Primitive (NegB x)
    }
  | INPUT l=delimited(DOUBLE_QUOTE, ID, DOUBLE_QUOTE)
    {
      Primitive (Input l)
    }
  | PROJ i=INT m=INT t=atomic_term
    {
      Primitive (Proj (i, m, t))
    }
  | CONS t=atomic_term u=atomic_term
    {
      Primitive (Cons (t, u))
    }
  | MAP t=atomic_term u=atomic_term
    {
      Primitive (Map (t, u))
    }
  | ITER t=atomic_term u=atomic_term
    {
      Primitive (Iter (t, u))
    }
  | FOLD_LEFT t=atomic_term u=atomic_term v=atomic_term
    {
      Primitive (FoldL (t, u, v))
    }
  | FOLD_RIGHT t=atomic_term u=atomic_term v=atomic_term
    {
      Primitive (FoldR (t, u, v))
    }
  | t=simple_term AT u=atomic_term
    {
      At (t, u)
    }
  | t=atomic_term
    {
      t
    }

atomic_term:
  | xs=tuple
    {
      tuple_or_term xs
    }
  | xs=tlist
    {
      make_list xs
    }
  | x=literal_or_var
    {
      x
    }

literal_or_var:
  | x=ID
    {
      Var x
    }
  | NIL
    {
      Const Nil
    }
  | b=TRUE
  | b=FALSE
    {
      Const (Bool b)
    }
  | x=INT
    {
      Const (Int x)
    }

%inline tlist:
  | LBRACKET xs=separated_list (SEMI_COLON, expression) RBRACKET { xs }

%inline tuple:
  | LPAREN xs=separated_list (COMMA, expression) RPAREN { xs }

%inline binop:
  | PLUS   { PPlus   }
  | MINUS  { PMinus  }
  | MULT   { PTimes  }
  | DIV    { PDiv    }
  | EQUAL  { PEq     }
  | NEQUAL { PNeq    }
  | LAND   { PAndB   }
  | LOR    { POrB    }
