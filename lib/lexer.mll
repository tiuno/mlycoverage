{ (* Emacs, open this with -*- tuareg -*- *)
  open Parser

let enter_newline lexbuf =
  Lexing.new_line lexbuf;
  lexbuf

exception SyntaxError of string
}

let newline = ('\013' | "\013\010")

let whitespace = [ '\032' '\009' '\010' ]

let alpha = ['a'-'z''A'-'Z']

let digit = ['0'-'9']

let sym  = ['_']

let ident = alpha (alpha | digit | sym)*

rule token = parse
| newline     { enter_newline lexbuf |> token }
| whitespace  { token lexbuf          }
| digit+ as d { INT (int_of_string d) }
| "nil"       { NIL                   }
| "cons"      { CONS                  }
| "map"       { MAP                   }
| "iter"      { ITER                  }
| "fold_left" { FOLD_LEFT             }
| "fold_right"{ FOLD_RIGHT            }
| "input"     { INPUT                 }
| "true"      { TRUE true             }
| "false"     { FALSE false           }
| "rectup"    { MUTUP                 }
| "="         { EQUAL                 }
| "<>"        { NEQUAL                }
| "and"       { AND                   }
| "let"       { LET                   }
| "fun"       { FUN                   }
| "proj"      { PROJ                  }
| "if"        { IF                    }
| "then"      { THEN                  }
| "else"      { ELSE                  }
| "::"        { FBY                   }
| "@"         { AT                    }
| "->"        { ARROW                 }
| ","         { COMMA                 }
| "("         { LPAREN                }
| ")"         { RPAREN                }
| ";"         { SEMI_COLON            }
| "\""        { DOUBLE_QUOTE          }
| "["         { LBRACKET              }
| "]"         { RBRACKET              }
| "not"       { NOT                   }
| "+"         { PLUS                  }
| "-"         { MINUS                 }
| "*"         { MULT                  }
| "/"         { DIV                   }
| "&&"        { LAND                  }
| "||"        { LOR                   }
| ident as id { ID id                 }
| eof         { EOF                   }
| _
      { raise (SyntaxError ("Syntax Error: " ^ Lexing.lexeme lexbuf))}
