open Mlycoverage

let process_expression ~lexer_init ~input =
  try Parser.exp Lexer.token (lexer_init input) with
  | Sys_error msg -> failwith (Format.asprintf "%s during parsing." msg)

let parse_expression =
  let parse lexer_init input = process_expression ~lexer_init ~input in
  let parse_string = parse Lexing.from_string in
  parse_string

let process_bindings ~lexer_init ~input =
  try Parser.program Lexer.token (lexer_init input) with
  | Sys_error msg -> failwith (Format.asprintf "%s during parsing." msg)

let parse_binding =
  let parse lexer_init input = process_bindings ~lexer_init ~input in
  parse Lexing.from_string

let ppf_term ppf t =
  match t with
  | Syntax.TypedExp (t, ty) ->
     Format.fprintf ppf " - : %a = %a@." Printer.fmt_of_type ty Printer.fmt_of_term t
  | _ ->
     Format.fprintf ppf "%a" Printer.fmt_of_term t

let print_binding ppf binding =
  match binding with
  | Syntax.Def (x, t) ->
     Format.fprintf ppf "let %s = %a\n" x ppf_term t
  | Syntax.RecValues xs ->
     Format.fprintf ppf "%a" ppf_term (Syntax.MuTuple xs)

let parse_print s = parse_expression s |> Printer.string_of_term

let parse_print_binding s =
  parse_binding s |> List.iter (print_binding Format.str_formatter);
  Format.flush_str_formatter ()

let parse_int () = Alcotest.(check string) "same string" "1" (parse_print "1")

let parse_int2 () =
  Alcotest.(check string) "same string" "23424564" (parse_print "23424564")

let parse_unit () =
  Alcotest.(check string) "same string" "()" (parse_print "()")

let parse_nil () = Alcotest.(check string) "same string" "[]" (parse_print "[]")

let parse_true () =
  Alcotest.(check string) "same string" "true" (parse_print "true")

let parse_false () =
  Alcotest.(check string) "same string" "false" (parse_print "false")

let parse_tuple () =
  Alcotest.(check string) "same string" "(x, y, z)" (parse_print "(x,y,z)")

let parse_projnm1 () =
  Alcotest.(check string) "same string" "proj 1 2 x" (parse_print "proj 1 2 x")

let parse_projnm2 () =
  Alcotest.(check string)
    "same string" "proj 3 3 (x, y, z)"
    (parse_print "proj 3 3 (x, y, z)")

let parse_artih_plus () =
  Alcotest.(check string) "same string" "x + y" (parse_print "x + y")

let parse_artih_minus () =
  Alcotest.(check string) "same string" "42 - 43" (parse_print "42 - 43")

let parse_artih_times () =
  Alcotest.(check string) "same string" "42 * x" (parse_print "42 * x")

let parse_artih_div () =
  Alcotest.(check string) "same string" "y / 1" (parse_print "y / 1")

let parse_bool_neg () =
  Alcotest.(check string) "same string" "not a" (parse_print "not a")

let parse_bool_and () =
  Alcotest.(check string) "same string" "x && true" (parse_print "x && true")

let parse_bool_or () =
  Alcotest.(check string) "same string" "false || y" (parse_print "false || y")

let parse_bool_eq () =
  Alcotest.(check string) "same string" "x = y" (parse_print "x = y")

let parse_bool_neq () =
  Alcotest.(check string)
    "same string" "true <> false"
    (parse_print "true <> false")

let parse_list () =
  Alcotest.(check string) "same string" "[1; 2]" (parse_print "[1;2]")

let parse_list_cons () =
  Alcotest.(check string) "same string"
    "cons 0 (cons 1 x)"
    (parse_print "cons 0 (cons 1 x)")

let parse_list_map () =
  Alcotest.(check string)
    "same string" "map succ [1; 2]"
    (parse_print "map succ [1;2]")

let parse_list_iter () =
  Alcotest.(check string)
    "same string" "iter (fun x -> ()) []"
    (parse_print "iter (fun x -> ()) []")

let parse_list_foldl () =
  Alcotest.(check string)
    "same string" "fold_left (fun x -> fun acc -> x + acc) 0 xs"
    (parse_print "fold_left (fun x -> (fun acc -> x + acc)) 0 xs")

let parse_list_foldr () =
  Alcotest.(check string)
    "same string" "fold_right (fun acc -> fun x -> acc * x) [1; 2; 3] 1"
    (parse_print "fold_right (fun acc -> (fun x -> acc * x)) [1;2;3] 1")

let parse_if_then_else () =
  Alcotest.(check string)
    "same string" "if condition then branch_one else branch_two"
    (parse_print "if condition then branch_one else branch_two")

let parse_if_then_else2 () =
  Alcotest.(check string)
    "same string"
    "if true then (if false then 1 else 2) else 3"
    (parse_print "if true then if false then 1 else 2 else 3")

let parse_if_then_else3 () =
  Alcotest.(check string)
    "same string"
    "if true then 2 else (if false then 1 else 3)"
    (parse_print "if true then 2 else if false then 1 else 3")

let parse_if_then_else4 () =
  Alcotest.(check string)
    "same string"
    "if (if true then true else false) then 2 else 3"
    (parse_print "if if true then true else false then 2 else 3")

let parse_rectup () =
  Alcotest.(check string)
    "same string" "rectup x = x"
    (parse_print "rectup x = x")

let parse_rectup_input () =
  Alcotest.(check string)
    "same string" "rectup x = input \"x\"::x"
    (parse_print "rectup x = input \"x\":: x")

let parse_rectup_id () =
  Alcotest.(check string)
    "same string" "rectup id = fun x -> x::id"
    (parse_print "rectup id = fun x -> x::id")

let parse_rectup_app () =
  Alcotest.(check string)
    "same string" "rectup f = g h x"
    (parse_print "rectup f = g h x")

let parse_rectup_paren_app () =
  Alcotest.(check string)
    "same string" "rectup f = g (h x)"
    (parse_print "rectup f = g (h x)")

let parse_rectup_int_stream () =
  Alcotest.(check string)
    "same string" "rectup x = 1::x"
    (parse_print "rectup x = 1 :: x")

let parse_rectup_nats () =
  Alcotest.(check string)
    "same string"
    "rectup nats =\n\
    \ 0::proj 0 1 (rectup next = (nats @ (fun x -> x - 1)) + 1::next)"
    (parse_print
       "rectup nats = 0 ::\n\
       \        proj 0 1 (rectup next = (nats @ (fun x -> x - 1)) + 1 :: next)")

let parse_rectup_mutual () =
  Alcotest.(check string)
    "same string" "rectup x = 0::y\nand y = 1::x"
    (parse_print "rectup x = 0::y and y = 1::x")

let parse_fun () =
  Alcotest.(check string) "same string" "fun x -> x" (parse_print "fun x -> x")

let parse_app () =
  Alcotest.(check string) "same string" "m n" (parse_print "m n")

let parse_app_fun () =
  Alcotest.(check string)
    "same string" "(fun x -> x) y"
    (parse_print "(fun x -> x) y")

let parse_binding () =
  Alcotest.(check string)
    "same string" "let x = 1\n"
    (parse_print_binding "let x = 1")

let () =
  Alcotest.run "Lisa parser"
    [
      ( "Literals",
        [
          Alcotest.test_case "integers" `Quick parse_int;
          Alcotest.test_case "integers" `Quick parse_int2;
          Alcotest.test_case "unit" `Quick parse_unit;
          Alcotest.test_case "list" `Quick parse_nil;
          Alcotest.test_case "bool" `Quick parse_true;
          Alcotest.test_case "bool" `Quick parse_false;
          Alcotest.test_case "tuples" `Quick parse_tuple;
        ] );
      ( "Primitives",
        [
          Alcotest.test_case "tuples" `Quick parse_projnm1;
          Alcotest.test_case "tuples" `Quick parse_projnm2;
          Alcotest.test_case "arith" `Quick parse_artih_plus;
          Alcotest.test_case "arith" `Quick parse_artih_minus;
          Alcotest.test_case "arith" `Quick parse_artih_times;
          Alcotest.test_case "arith" `Quick parse_artih_div;
          Alcotest.test_case "bool" `Quick parse_bool_neg;
          Alcotest.test_case "bool" `Quick parse_bool_and;
          Alcotest.test_case "bool" `Quick parse_bool_or;
          Alcotest.test_case "bool" `Quick parse_bool_eq;
          Alcotest.test_case "bool" `Quick parse_bool_neq;
          Alcotest.test_case "list" `Quick parse_list;
          Alcotest.test_case "list" `Quick parse_list_cons;
          Alcotest.test_case "list" `Quick parse_list_map;
          Alcotest.test_case "list" `Quick parse_list_iter;
          Alcotest.test_case "list" `Quick parse_list_foldr;
          Alcotest.test_case "list" `Quick parse_list_foldl;
          Alcotest.test_case "if" `Quick parse_if_then_else;
          Alcotest.test_case "if" `Quick parse_if_then_else2;
          Alcotest.test_case "if" `Quick parse_if_then_else3;
          Alcotest.test_case "if" `Quick parse_if_then_else4;
          Alcotest.test_case "input" `Quick parse_rectup_input;
        ] );
      ( "Bindings",
        [
          Alcotest.test_case "rectup" `Quick parse_rectup;
          Alcotest.test_case "rectup id" `Quick parse_rectup_id;
          Alcotest.test_case "rectup application" `Quick parse_rectup_app;
          Alcotest.test_case "rectup parenthesized application" `Quick
            parse_rectup_paren_app;
          Alcotest.test_case "rectup int stream" `Quick parse_rectup_int_stream;
          Alcotest.test_case "rectup nats" `Quick parse_rectup_nats;
          Alcotest.test_case "rectup mutual" `Quick parse_rectup_mutual;
          Alcotest.test_case "let" `Quick parse_binding;
        ] );
      ( "Term",
        [
          Alcotest.test_case "function" `Quick parse_fun;
          Alcotest.test_case "application" `Quick parse_app;
          Alcotest.test_case "application" `Quick parse_app_fun;
        ] );
    ]
