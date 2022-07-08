open Eval
open Parser

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try(let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = eval_decl env decl in
  Printf.printf "val %s = " id;
  pp_val v;
  print_newline();
  read_eval_print newenv) (*3.2.2 Evalで発生したエラーをtryでキャッチする*) with 
  Eval.Error e -> print_string e;print_newline();read_eval_print env (*3.2.2 Evalでエラーが発生したら対応するエラーメッセージを出力してインタプリタプロンプトに戻るようにする。*)
  |Parser.Error -> print_string "Parser.error";print_newline();read_eval_print env (*3.2.2 Parserでエラーが発生したらEval.errorと出力してインタプリタプロンプトに戻るようにする。*)
  |_ -> print_string "Eval、Parser以外でのエラー";print_newline();read_eval_print env (*3.2.2 lexerなどでエラーが発生したらEval.errorと出力してインタプリタプロンプトに戻るようにする。*)

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "ii" (IntV 2) (*3.2.1 ii が 2，iii が 3，iv が 4 となるようにプログラムを変更*)
      (Environment.extend "iii" (IntV 3)
        (Environment.extend "iv" (IntV 4)
          (Environment.extend "v" (IntV 5)
            (Environment.extend "x" (IntV 10) Environment.empty)))))