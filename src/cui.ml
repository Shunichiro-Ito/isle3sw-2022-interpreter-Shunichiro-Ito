open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = eval_decl env decl in
  Printf.printf "val %s = " id;
  pp_val v;
  print_newline();
  read_eval_print newenv

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "ii" (IntV 2)
      (Environment.extend "iii" (IntV 3)
        (Environment.extend "iv" (IntV 4)
          (Environment.extend "v" (IntV 5)
            (Environment.extend "x" (IntV 10) Environment.empty)))))
              
    
open Typing
open Syntax

let rec read_eval_print env tyenv = (* New! 型環境を REPL で保持 *)
   print_string "# ";
   flush stdout;
   let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
   let ty = ty_decl tyenv decl in (* New! let 宣言のための型推論 *)
   let (id, newenv, v) = eval_decl env decl in
     (* New! 型を出力するように変更 *)
     Printf.printf "val %s : " id;
     pp_ty ty;
     print_string " = ";
     pp_val v;
     print_newline();
     (* 型環境を次のループに渡す．let 宣言はまだないので，tyenv を新しくする必要はない． *)
     read_eval_print newenv tyenv 

(* New! initial_env のための型環境を作る *)
let initial_tyenv =
   Environment.extend "i" TyInt
     (Environment.extend "v" TyInt
       (Environment.extend "x" TyInt Environment.empty))