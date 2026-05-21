open Ast

let indent level = String.make (2 * (level - 1)) ' '

let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\b' -> Buffer.add_string buf "\\b"
      | '\x0c' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\'' -> Buffer.add_string buf "\\'"
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Main parsing loop *)
let rec unparse_program decls =
  String.concat "" (List.map (unparse_decl 1) decls)

and unparse_decl level = function
  | GlobalVarDecl (t, Ident id, init) ->
      indent level ^ unparse_type t ^ " " ^ id ^ unparse_array_suffix t
      ^ unparse_init init ^ ";\n"
  | LocalVarDecl (t, Ident id, init) ->
      indent level ^ unparse_type t ^ " " ^ id ^ unparse_array_suffix t
      ^ unparse_init init ^ ";\n"
  | FuncDecl (t, Ident id, para_list, body) ->
      indent level ^ unparse_type t ^ " " ^ id ^ "("
      ^ unparse_para_list para_list
      ^ ")\n" ^ unparse_stmt level body

and unparse_type = function
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "boolean"
  | VoidType -> "void"
  | ArrayType (base, _) -> unparse_type base
  | _ -> ""

and unparse_init = function EmptyExpr -> "" | e -> " = " ^ unparse_expr e

and unparse_para_list paras =
  String.concat ", "
    (List.map
       (fun (ParaDecl (t, Ident id)) ->
         unparse_type t ^ " " ^ id ^ unparse_array_suffix t)
       paras)

and unparse_array_suffix = function
  | ArrayType (_, size) ->
      "[" ^ (match size with EmptyExpr -> "" | _ -> unparse_expr size) ^ "]"
  | _ -> ""

and unparse_arg_list args =
  String.concat ", " (List.map (fun (Arg e) -> unparse_expr e) args)

and unparse_expr = function
  | IntExpr e -> e
  | FloatExpr e -> e
  | BoolExpr e -> e
  | StringExpr e -> "\"" ^ escape e ^ "\""
  | EmptyExpr -> ""
  | AssignExpr (e1, e2) -> "(" ^ unparse_expr e1 ^ "=" ^ unparse_expr e2 ^ ")"
  | BinaryExpr (e1, Operator op, e2) ->
      "(" ^ unparse_expr e1 ^ op ^ unparse_expr e2 ^ ")"
  | UnaryExpr (Operator op, e) -> op ^ unparse_expr e
  | VarExpr (SimpleVar (Ident id)) -> id
  | ArrayExpr (SimpleVar (Ident id), index) ->
      id ^ "[" ^ unparse_expr index ^ "]"
  | CallExpr (Ident id, args) -> id ^ "(" ^ unparse_arg_list args ^ ")"
  | ArrayInitExpr exprs ->
      "{" ^ String.concat "," (List.map unparse_expr exprs) ^ "}"

and unparse_stmt level = function
  | EmptyStmt -> ""
  | BreakStmt -> indent level ^ "break;\n"
  | ContinueStmt -> indent level ^ "continue;\n"
  | ReturnStmt e -> indent level ^ "return " ^ unparse_expr e ^ ";\n"
  | ExprStmt e -> indent level ^ unparse_expr e ^ ";\n"
  | EmptyCompStmt -> indent level ^ "{\n" ^ indent level ^ "}\n"
  | CompoundStmt (decls, stmts) ->
      indent level ^ "{\n"
      ^ String.concat "" (List.map (unparse_decl (level + 1)) decls)
      ^ String.concat "" (List.map (unparse_stmt (level + 1)) stmts)
      ^ indent level ^ "}\n"
  | WhileStmt (cond, body) ->
      let body_str =
        match body with
        | CompoundStmt _ -> "\n" ^ unparse_stmt level body
        | _ -> "\n" ^ unparse_stmt (level + 1) body
      in
      indent level ^ "while (" ^ unparse_expr cond ^ ")" ^ body_str
  | IfStmt (cond, then_branch, else_branch) ->
      let then_str =
        match then_branch with
        | CompoundStmt _ | EmptyCompStmt ->
            "\n" ^ unparse_stmt level then_branch
        | _ -> "\n" ^ unparse_stmt (level + 1) then_branch
      in
      let base = indent level ^ "if (" ^ unparse_expr cond ^ ")" ^ then_str in
      begin match else_branch with
      | EmptyStmt -> base
      | IfStmt _ ->
          let inner_str = unparse_stmt level else_branch in
          let prefix_len = String.length (indent level) in
          let clean_inner =
            String.sub inner_str prefix_len
              (String.length inner_str - prefix_len)
          in
          base ^ indent level ^ "else " ^ clean_inner
      | _ ->
          let else_str =
            match else_branch with
            | CompoundStmt _ | EmptyCompStmt ->
                "\n" ^ unparse_stmt level else_branch
            | _ -> "\n" ^ unparse_stmt (level + 1) else_branch
          in
          base ^ indent level ^ "else" ^ else_str
      end
  | ForStmt (e1, e2, e3, body) ->
      let body_str =
        match body with
        | CompoundStmt _ -> "\n" ^ unparse_stmt level body
        | _ -> "\n" ^ unparse_stmt (level + 1) body
      in
      indent level ^ "for (" ^ unparse_expr e1 ^ ";" ^ unparse_expr e2 ^ ";"
      ^ unparse_expr e3 ^ ")" ^ body_str
