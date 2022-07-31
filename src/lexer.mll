{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("fun", Parser.FUN);
  ("dfun", Parser.DFUN);
  ("rec",Parser.REC)
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "=" { Parser.EQ } 
| "->" { Parser.RARROW } (* New! *)
| "&&" { Parser.AND } (*3.2.3 &&の実装*)
| "||" { Parser.OR } (*3.2.3 ||の実装*)
| "(*"  { comment 1 lexbuf } (*コメントアウトの開始記号を受け取ったらcommentに移動する*)

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

and comment n = parse (*3.2.4 コメントアウトをスルーするための追加部分*)
"(*" {let n = n + 1 in comment n lexbuf}
|"*)" {let n = n - 1 in (if n = 0 then main lexbuf else comment n lexbuf)} (*引数の値が0になったら元の字句解析に戻る*)
|_ { comment n lexbuf }

