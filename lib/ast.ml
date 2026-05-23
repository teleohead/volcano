type program = Program of decl list

and decl =
  | FuncDecl of typ * ident * para_decl list * stmt
  | GlobalVarDecl of typ * ident * expr
  | LocalVarDecl of typ * ident * expr

and para_decl = ParaDecl of typ * ident

and stmt =
  | CompoundStmt of decl list * stmt list
  | IfStmt of expr * stmt * stmt
  | ForStmt of expr * expr * expr * stmt
  | WhileStmt of expr * stmt
  | ExprStmt of expr
  | ReturnStmt of expr
  | BreakStmt
  | ContinueStmt
  | EmptyCompStmt
  | EmptyStmt

and expr =
  | VarExpr of var
  | AssignExpr of expr * expr
  | BinaryExpr of expr * operator * expr
  | UnaryExpr of operator * expr
  | CallExpr of ident * expr list
  | IntExpr of string
  | BoolExpr of string
  | FloatExpr of string
  | StringExpr of string
  | ArrayExpr of var * expr
  | ArrayInitExpr of expr list
  | EmptyExpr

and typ =
  | IntType
  | FloatType
  | BoolType
  | StringType
  | VoidType
  | ArrayType of typ * expr
  | ErrorType

and var = SimpleVar of ident
and ident = Ident of string
and operator = Operator of string
