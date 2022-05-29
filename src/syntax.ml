(* ML interpreter / type reconstruction *)
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
let pp_ty typ =
  match typ with
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"

let freevar_ty _ =
  assert false (* Exercise 4.3.1 *)

let string_of_ty typ =
  match typ with
  TyInt -> "int"
| TyBool -> "bool"


let pp_ty ty =
  print_string (string_of_ty ty)
