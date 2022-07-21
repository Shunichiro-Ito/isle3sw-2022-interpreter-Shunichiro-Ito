open Syntax
open MySet
open Eval

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list
  

let hd (x::rest) = x (*listの先頭の要素を返す関数*)
let tl (x::rest) = rest (*listの先頭以外の要素を返す関数*)

let rec subst_type s ty = (*subst -> ty -> ty*)
  match s with
    [] -> ty
    | _ -> (match ty with
              TyFun(a,b) -> TyFun((subst_type s a), (subst_type s b))
              | TyVar x -> let(var, typ) = hd s in 
                           if x=var then subst_type (tl s) typ (*置き換えを行い、sの残りの要素についてsubst_typeを計算*)
                           else subst_type (tl s) ty (*置き換えを行わず、sの残りの要素についてsubst_typeを計算*)
              | _ -> ty)  

(* New! eqs_of_subst : subst -> (ty * ty) list
   型代入を型の等式集合に変換．型の等式制約 ty1 = ty2 は (ty1,ty2) という
   ペアで表現し，等式集合はペアのリストで表現． *)
let rec eqs_of_subst s = 
  match s with
    [] -> []
    |(tyvar, ty)::rest -> (TyVar tyvar, ty)::eqs_of_subst rest

(*subst_eqs: subst -> (ty * ty) list -> (ty * ty) list 型の等式集合に型代入を適用する関数． *)
let rec subst_eqs s eqs = 
  match eqs with
    [] -> []
    |(ty1,ty2)::rest -> (subst_type s ty1, subst_type s ty2)::(subst_eqs s rest) (*ty1,ty2に型代入を行い、リストの残りの要素についても再帰的に行う。*)

(*単一化を行う関数*)
let rec unify l = (* (ty * ty) list -> subst *)
  match l with
    [] -> [] (* 単一化の1番目*)
    |(ty1,ty2)::rest -> 
      if ty1 = ty2 then unify rest (* 単一化の2番目*)
      else (match ty1, ty2 with
        TyFun(ty11, ty12), TyFun(ty21, ty22) -> unify((ty11,ty21)::(ty12,ty22)::rest) (*単一化の3番目*)
        |TyVar tyvar1, _ (*単一化の4番目*) -> 
          if (member tyvar1  (freevar_ty ty2)) then err("Can not unify") (*オカーチェック*)
          else let subst_tys = [(tyvar1, ty2)] in
               let allsub = subst_eqs subst_tys rest in (*残りの制約中のαにτを代入した制約を求める*)
               subst_tys @ unify(allsub)
        |_, TyVar tyvar2 (*単一化の5番目*) -> 
          if (member tyvar2  (freevar_ty ty1)) then err("Can not unify") (*オカーチェック*)
          else let subst_tys = [(tyvar2, ty1)] in
               let allsub = subst_eqs subst_tys rest in (*残りの制約中のαにτを代入した制約を求める*)
               subst_tys @ unify(allsub)
        |_ -> err("Can not unify")) (*単一化の6番目*)

(* 演算子 op が生成すべき制約集合と返り値の型を記述 *)
let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
    | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
    | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)

(* 型環境 tyenv と式 exp を受け取って，型代入と exp の型のペアを返す関数　4.2.1実装時の内容
let rec ty_exp tyenv = function
    Var x -> 
      (try Environment.lookup x tyenv with
          Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
        ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      let tyarg3 = ty_exp tyenv exp3 in
      (match tyarg1, tyarg2, tyarg3 with
        TyBool, TyInt, TyInt -> TyInt
      | TyBool, TyBool, TyBool -> TyBool
      | _, _, _ -> err ("Argument must be of bool*'t*'t : if")
      )
  | LetExp (id, exp1, exp2) ->
      let value = ty_exp tyenv exp1 in ty_exp (Environment.extend id value tyenv)exp2
  | _ -> err ("Not Implemented!")
*)

(*型環境 tyenv と式 exp を受け取って，型代入と exp の型のペアを返す *)
let rec ty_exp tyenv exp =
  match exp with
    Var x ->
     (try ([], Environment.lookup x tyenv) with
         Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
	  (* s1 と s2 を等式制約の集合に変換して，eqs3 と合わせる *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
	  (* 全体の制約をもう一度解く．*)
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool)] @ [(ty2,ty3)] in
      let s4 = unify eqs in
      (s4, subst_type s4 ty2)
  | LetExp (id, exp1, exp2) ->
      let (s1,ty1) = ty_exp tyenv exp1 in
      let (s2,ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      (s3,subst_type s3 ty2)
  | FunExp (id, exp) ->
      (* id の型を表す fresh な型変数を生成 *)
      let domty = TyVar (fresh_tyvar ()) in
	  (* id : domty で tyenv を拡張し，その下で exp を型推論 *)
      let s, ranty =
        ty_exp (Environment.extend id domty tyenv) exp in
        (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) -> 
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let tyvar1 = TyVar (fresh_tyvar()) in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty1, TyFun(ty2, tyvar1))]in
      let s3 = unify eqs in
      (s3,subst_type s3 tyvar1)
  | _ -> err ("Not Implemented!")
  
let ty_decl tyenv = function
  Exp e -> let (_, ty) = ty_exp tyenv e in (tyenv, ty)
  | Decl (id, exp) -> let (_, ty) = ty_exp tyenv exp in 
                      let newtyenv = Environment.extend id ty tyenv in 
                      (newtyenv, ty)
  | _ -> err ("Not Implemented!")
