%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR (*3.2.3 tokenの追加*)
%token IF THEN ELSE TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID

%token LET IN EQ
%token REC
%token RARROW FUN DFUN

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET x=ID EQ e1=Expr p2=LetMultExpr SEMISEMI { MultDecl ((x, e1) :: p2) } 
  | LET REC x=ID EQ FUN y=ID RARROW e=Expr SEMISEMI { RecDecl (x, y, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e } 
  | e=LetRecExpr { e }
  | e=LTExpr { e }
  | e=FunExpr{ e }
  | e=DFunExpr{ e }
  | e=ANDExpr{ e }
  | e=ORExpr{ e }

LetRecExpr :(*3.5.1 再帰的関数の生成規則*)
    LET REC x=ID EQ FUN y=ID RARROW e1=Expr IN e2=Expr { LetRecExp ( x, y, e1, e2) }

FunExpr: (*3.4.1 fun式の生成規則*)
    FUN e1=ID RARROW e2=Expr { FunExp (e1, e2) }

DFunExpr:
    DFUN e1=ID RARROW e2=Expr { DFunExp (e1, e2) }

LetExpr : 
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) } 


LetMultExpr :(*3.3.2 letを並べた式の生成規則*)
    LET x=ID EQ e1=Expr p2=LetMultExpr { (x, e1) :: p2}
  | LET x=ID EQ e=Expr { [(x, e)] }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

ORExpr : (*3.2.3 ||の生成規則*)
    l=ORExpr OR r=ANDExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr : (*3.2.3 ||の生成規則*)
    l=ANDExpr AND r=LTExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }
 
MExpr :
    e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | e=AppExpr { e } 

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
