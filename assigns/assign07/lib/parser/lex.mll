{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
<<<<<<< HEAD
  | "()" { UNIT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "let" { LET }
  | "fun" { FUN }
  | "->" { TO }
  | "in" { IN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "mod" { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "=" { EQ }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "else" { ELSE }
  | "then" { THEN }
=======
>>>>>>> upstream/main
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
