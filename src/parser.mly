%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID

%token LET IN EQ
%token REC
%token RARROW FUN (* New! *)

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET REC x=ID EQ FUN y=ID RARROW e=Expr SEMISEMI { RecDecl (x, y, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e } 
  | e=LetRecExpr { e }
  | e=LTExpr { e }
  | e=FunExpr{ e }

LetRecExpr :
    LET REC x=ID EQ FUN y=ID RARROW e1=Expr IN e2=Expr { LetRecExp ( x, y, e1, e2) }

FunExpr:
    FUN e1=ID RARROW e2=Expr { FunExp (e1, e2) }

LetExpr : 
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) } 

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }
 
MExpr :
    e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | e=AppExpr { e } (* New! *)

(* New! *)
AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
