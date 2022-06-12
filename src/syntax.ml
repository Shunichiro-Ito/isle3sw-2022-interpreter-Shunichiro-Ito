(* ML interpreter / type reconstruction *)
open MySet
type id = string

type binOp = Plus | Mult | Lt

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp (* New! *)
  | AppExp of exp * exp (* New! *)
  | LetRecExp of id * id * exp * exp

type program =
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

(* ty 型の値のための pretty printer *)
let rec pp_ty typ =
  match typ with
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  
  
  (* New! 呼び出すたびに，他とかぶらない新しい tyvar 型の値を返す関数 *)

let fresh_tyvar =
  let counter = ref 0 in (* 次に返すべき tyvar 型の値を参照で持っておいて， *)
  let body () =
    let v = !counter in
      counter := v + 1; v (* 呼び出されたら参照をインクリメントして，古い counter の参照先の値を返す *)
  in body


let rec freevar_ty ty =
  match ty with
    TyVar var -> MySet.singleton var
    |TyFun (ty1,ty2) ->
      union (freevar_ty ty1) (freevar_ty ty2)
    |TyList ty1 -> freevar_ty ty1
    |_ -> MySet.empty
  

let rec string_of_ty typ =
  match typ with
  TyInt -> "int"
| TyBool -> "bool"
| TyVar tyvar -> 
  let n = tyvar / 26 in
   let id = tyvar - 26*n + 97 in
   let ch = char_of_int id in
   let st = Char.escaped ch in
   "'" ^ st ^ string_of_int n
| TyFun (ty1 , ty2) -> "(" ^ string_of_ty (ty1) ^ "->" ^ string_of_ty(ty2) ^ ")"


let pp_ty ty =
  print_string (string_of_ty ty)
