open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref 
  | DProcV of id * exp * dnval Environment.t ref 
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (x, e, v) -> " <fun> "
  | DProcV (x, e, v) -> " <dfun> "

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2) (*3.2.3 &&の計算を行う*)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2) (*3.2.3 ||の計算を行う*)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in(
      if(op = And && arg1 = BoolV false) then BoolV false  (*3.2.3 前半だけで評価値が決まる場合はそれを出力する*)
      else if(op = Or && arg1 = BoolV true) then BoolV true
      else let arg2 = eval_exp env exp2 in 
      apply_prim op arg1 arg2
    )
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) -> let value = eval_exp env exp1 in eval_exp (Environment.extend id value env)exp2
  (* 関数定義式: 現在の環境 env をクロージャ内に保存 *)
  | LetMultExp ((id, exp1), p2) -> let value = eval_exp env exp1 in eval_exp (Environment.extend id value env)p2
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  (* 関数適用式 *)
  | DFunExp (id, exp) -> DProcV (id, exp, ref env)
  | AppExp (exp1, exp2) ->
      (* 関数 exp1 を現在の環境で評価 *)
      let funval = eval_exp env exp1 in
      (* 実引数 exp2 を現在の環境で評価 *)
      let arg = eval_exp env exp2 in
      (* 関数 exp1 の評価結果をパターンマッチで取り出す *)
      (match funval with
          ProcV (id, body, env') -> ((* 評価結果が実際にクロージャであれば *)
              (* クロージャ内の環境を取り出して仮引数に対する束縛で拡張 *)
              let newenv = Environment.extend id arg !env' in
                eval_exp newenv body)
          |DProcV (id, body, env') -> 
            let dummyenv = ref Environment.empty in
            let newenv = Environment.extend id (ProcV (id, exp1, dummyenv)) !env' in
            eval_exp newenv body
          | _ -> 
            (* 評価結果がクロージャでなければ，実行時型エラー *)
            err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) ->
    (* ダミーの環境への参照を作る *)
    let dummyenv = ref Environment.empty in
    (* 関数閉包を作り，idをこの関数閉包に写像するように現在の環境envを拡張 *)
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
    (* ダミーの環境への参照に，拡張された環境を破壊的代入してバックパッチ *)
        dummyenv := newenv;
        eval_exp newenv exp2

let rec eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) -> let v = eval_exp env e in (id, Environment.extend id v env, v)
  | MultDecl (p) -> (*3.3.2 let宣言の列を処理する*)
    (match p with
      [(x,e)] -> eval_decl env (Decl (x,e))
      |((x,e1)::p2) -> let v = eval_exp env e1 in eval_decl (Environment.extend x v env) (MultDecl (p2))
    )
  | RecDecl (id, para, e) ->
    let dummyenv = ref Environment.empty in
    let newenv = Environment.extend id (ProcV (para, e, dummyenv)) env in
    let v =  (ProcV(para, e, dummyenv)) in 
    dummyenv := newenv;
    (id, newenv, v)
  