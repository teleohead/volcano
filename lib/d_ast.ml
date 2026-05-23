open Ast

type d_program = D_Program of d_decl list

and d_decl =
  | D_FuncDecl of typ * ident * para_decl list * d_stmt
  | D_GlobalVarDecl of typ * ident * d_expr
  | D_LocalVarDecl of typ * ident * d_expr

and d_stmt =
  | D_CompoundStmt of d_decl list * d_stmt list
  | D_IfStmt of d_expr * d_stmt * d_stmt
  | D_ForStmt of d_expr * d_expr * d_expr * d_stmt
  | D_WhileStmt of d_expr * d_stmt
  | D_ExprStmt of d_expr
  | D_ReturnStmt of d_expr
  | D_BreakStmt
  | D_ContinueStmt
  | D_EmptyCompStmt
  | D_EmptyStmt

and d_expr = { de_kind : d_expr_kind; de_type : typ }

and d_expr_kind =
  | D_VarExpr of var * d_decl
  | D_AssignExpr of d_expr * d_expr
  | D_BinaryExpr of d_expr * d_operator * d_expr
  | D_UnaryExpr of d_operator * d_expr
  | D_CallExpr of ident * d_expr list * d_decl
  | D_IntExpr of string
  | D_BoolExpr of string
  | D_FloatExpr of string
  | D_StringExpr of string
  | D_ArrayExpr of var * d_expr * d_decl
  | D_ArrayInitExpr of d_expr list
  | D_ImplicitCastExpr of d_expr
  | D_EmptyExpr

and d_operator = IntOp of string | FloatOp of string | RawOp of string
