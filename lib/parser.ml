open Token
open Ast

type tokens = Token.t list

(* Helper Functions *)

let current_kind = function token :: _ -> token.kind | [] -> EOF

let current_token = function
  | token :: _ -> token
  | [] -> Token.make EOF "$" Source_position.dummy

let advance = function _ :: tail -> tail | [] -> []

let parse_error msg tokens =
  Errors.report (Compile_error.make msg (current_token tokens).position);
  failwith "parse error"

let expect kind tokens =
  match current_kind tokens with
  | k when k = kind -> advance tokens
  | _ -> parse_error ("\"" ^ string_of_kind kind ^ "\" expected here") tokens

(* Primitives *)

let parse_type tokens =
  match current_kind tokens with
  | Int -> (IntType, expect Int tokens)
  | Float -> (FloatType, expect Float tokens)
  | Boolean -> (BoolType, expect Boolean tokens)
  | Void -> (VoidType, expect Void tokens)
  | _ -> parse_error "type expected here" tokens

let parse_ident tokens =
  match tokens with
  | id :: tail when id.kind = Id -> (Ident id.spelling, tail)
  | _ -> parse_error "identifier expected here" tokens

let parse_operator tokens =
  let op = current_token tokens in
  (Operator op.spelling, advance tokens)

let parse_int_lit tokens =
  match tokens with
  | lit :: tail when lit.kind = IntLit -> (IntExpr lit.spelling, tail)
  | _ -> parse_error "integer literal expected here" tokens

let parse_float_lit tokens =
  match tokens with
  | lit :: tail when lit.kind = FloatLit -> (FloatExpr lit.spelling, tail)
  | _ -> parse_error "float literal expected here" tokens

let parse_bool_lit tokens =
  match tokens with
  | lit :: tail when lit.kind = BoolLit -> (BoolExpr lit.spelling, tail)
  | _ -> parse_error "boolean literal expected here" tokens

let parse_string_lit tokens =
  match tokens with
  | lit :: tail when lit.kind = StringLit -> (StringExpr lit.spelling, tail)
  | _ -> parse_error "string literal expected here" tokens

(* Expressions *)

let rec parse_expr tokens = parse_assignment_expr tokens

and parse_assignment_expr tokens =
  let lhs, tokens = parse_cond_or_expr tokens in
  match current_kind tokens with
  | Eq ->
      let tokens = expect Eq tokens in
      let rhs, tokens = parse_assignment_expr tokens in
      (AssignExpr (lhs, rhs), tokens)
  | _ -> (lhs, tokens)

and parse_cond_or_expr tokens =
  let init, tokens = parse_cond_and_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | OrOr ->
        let op, tokens = parse_operator tokens in
        let rhs, tokens = parse_cond_and_expr tokens in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc tokens
    | _ -> (acc, tokens)
  in
  loop init tokens

and parse_cond_and_expr tokens =
  let init, tokens = parse_equality_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | AndAnd ->
        let op, tokens = parse_operator tokens in
        let rhs, tokens = parse_equality_expr tokens in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc tokens
    | _ -> (acc, tokens)
  in
  loop init tokens

and parse_equality_expr tokens =
  let init, tokens = parse_rel_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | EqEq | NotEq ->
        let op, tokens = parse_operator tokens in
        let rhs, tokens = parse_rel_expr tokens in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc tokens
    | _ -> (acc, tokens)
  in
  loop init tokens

and parse_rel_expr tokens =
  let init, tokens = parse_additive_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | Lt | Gt | LtEq | GtEq ->
        let op, tokens = parse_operator tokens in
        let rhs, tokens = parse_additive_expr tokens in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc tokens
    | _ -> (acc, tokens)
  in
  loop init tokens

and parse_additive_expr tokens =
  let init, tokens = parse_multiplicative_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | Plus | Minus ->
        let op, tokens = parse_operator tokens in
        let rhs, tokens = parse_multiplicative_expr tokens in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc tokens
    | _ -> (acc, tokens)
  in
  loop init tokens

and parse_multiplicative_expr tokens =
  let init, tokens = parse_unary_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | Mult | Div ->
        let op, tokens = parse_operator tokens in
        let rhs, tokens = parse_unary_expr tokens in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc tokens
    | _ -> (acc, tokens)
  in
  loop init tokens

and parse_unary_expr tokens =
  match current_kind tokens with
  | Plus | Minus | Not ->
      let op, tokens = parse_operator tokens in
      let operand, tokens = parse_unary_expr tokens in
      (UnaryExpr (op, operand), tokens)
  | _ -> parse_primary_expr tokens

and parse_primary_expr tokens =
  match current_kind tokens with
  | Id -> (
      let id, tokens = parse_ident tokens in
      match current_kind tokens with
      | LParen ->
          let tokens = expect LParen tokens in
          let args, tokens = parse_arg_list tokens in
          (CallExpr (id, args), tokens)
      | LBracket ->
          let tokens = expect LBracket tokens in
          let index, tokens = parse_expr tokens in
          let tokens = expect RBracket tokens in
          (ArrayExpr (SimpleVar id, index), tokens)
      | _ -> (VarExpr (SimpleVar id), tokens))
  | LParen ->
      let tokens = expect LParen tokens in
      let expr, tokens = parse_expr tokens in
      let tokens = expect RParen tokens in
      (expr, tokens)
  | IntLit -> parse_int_lit tokens
  | FloatLit -> parse_float_lit tokens
  | BoolLit -> parse_bool_lit tokens
  | StringLit -> parse_string_lit tokens
  | _ ->
      parse_error ("\"" ^ string_of_kind Semicolon ^ "\" expected here") tokens

(* Arguments *)

and parse_arg tokens =
  let expr, tokens = parse_expr tokens in
  (Arg expr, tokens)

and parse_arg_list tokens =
  match current_kind tokens with
  | RParen -> ([], expect RParen tokens)
  | _ ->
      let proper_list, tokens = parse_proper_arg_list tokens in
      let tokens = expect RParen tokens in
      (proper_list, tokens)

and parse_proper_arg_list tokens =
  let arg, tokens = parse_arg tokens in
  match current_kind tokens with
  | Comma ->
      let tokens = expect Comma tokens in
      let list, tokens = parse_proper_arg_list tokens in
      (arg :: list, tokens)
  | _ -> (arg :: [], tokens)

(* Parameters *)

let parse_para_decl tokens =
  let type_, tokens = parse_type tokens in
  let id, tokens = parse_ident tokens in
  match current_kind tokens with
  | LBracket ->
      let tokens = expect LBracket tokens in
      let index, tokens =
        match current_kind tokens with
        | IntLit -> parse_int_lit tokens
        | _ -> (EmptyExpr, tokens)
      in
      let tokens = expect RBracket tokens in
      (ParaDecl (ArrayType (type_, index), id), tokens)
  | _ -> (ParaDecl (type_, id), tokens)

let rec parse_proper_para_list tokens =
  let para, tokens = parse_para_decl tokens in
  match current_kind tokens with
  | Comma ->
      let tokens = expect Comma tokens in
      let list, tokens = parse_proper_para_list tokens in
      (para :: list, tokens)
  | _ -> (para :: [], tokens)

let parse_para_list tokens =
  match current_kind tokens with
  | RParen -> ([], expect RParen tokens)
  | _ ->
      let proper_list, tokens = parse_proper_para_list tokens in
      let tokens = expect RParen tokens in
      (proper_list, tokens)

(* Statements *)

let rec parse_stmt tokens =
  match current_kind tokens with
  | LCurly -> parse_compound_stmt tokens
  | If -> parse_if_stmt tokens
  | For -> parse_for_stmt tokens
  | While -> parse_while_stmt tokens
  | Return -> parse_return_stmt tokens
  | Break -> parse_break_stmt tokens
  | Continue -> parse_continue_stmt tokens
  | _ -> parse_expr_stmt tokens

and parse_stmt_list tokens =
  match current_kind tokens with
  | RCurly -> ([], tokens)
  | _ ->
      let stmt, tokens = parse_stmt tokens in
      let tail, tokens = parse_stmt_list tokens in
      (stmt :: tail, tokens)

and parse_compound_stmt tokens =
  let tokens = expect LCurly tokens in
  let decls, tokens = parse_local_decl_list tokens in
  let stmts, tokens = parse_stmt_list tokens in
  let tokens = expect RCurly tokens in
  match (decls, stmts) with
  | [], [] -> (EmptyCompStmt, tokens)
  | _ -> (CompoundStmt (decls, stmts), tokens)

and parse_if_stmt tokens =
  let tokens = expect If tokens in
  let tokens = expect LParen tokens in
  let cond_expr, tokens = parse_expr tokens in
  let tokens = expect RParen tokens in
  let then_stmt, tokens = parse_stmt tokens in
  let else_stmt, tokens =
    match current_kind tokens with
    | Else ->
        let tokens = expect Else tokens in
        parse_stmt tokens
    | _ -> (EmptyStmt, tokens)
  in

  (IfStmt (cond_expr, then_stmt, else_stmt), tokens)

and parse_for_stmt tokens =
  let tokens = expect For tokens in
  let tokens = expect LParen tokens in

  let e1, tokens =
    match current_kind tokens with
    | Semicolon -> (EmptyExpr, tokens)
    | _ -> parse_expr tokens
  in
  let tokens = expect Semicolon tokens in

  let e2, tokens =
    match current_kind tokens with
    | Semicolon -> (EmptyExpr, tokens)
    | _ -> parse_expr tokens
  in
  let tokens = expect Semicolon tokens in

  let e3, tokens =
    match current_kind tokens with
    | RParen -> (EmptyExpr, tokens)
    | _ -> parse_expr tokens
  in
  let tokens = expect RParen tokens in

  let body_stmt, tokens = parse_stmt tokens in

  (ForStmt (e1, e2, e3, body_stmt), tokens)

and parse_while_stmt tokens =
  let tokens = expect While tokens in
  let tokens = expect LParen tokens in
  let cond_expr, tokens = parse_expr tokens in
  let tokens = expect RParen tokens in
  let body_stmt, tokens = parse_stmt tokens in

  (WhileStmt (cond_expr, body_stmt), tokens)

and parse_break_stmt tokens =
  let tokens = expect Break tokens in
  let tokens = expect Semicolon tokens in

  (BreakStmt, tokens)

and parse_continue_stmt tokens =
  let tokens = expect Continue tokens in
  let tokens = expect Semicolon tokens in

  (ContinueStmt, tokens)

and parse_return_stmt tokens =
  let tokens = expect Return tokens in
  let expr, tokens =
    match current_kind tokens with
    | Semicolon -> (EmptyExpr, tokens)
    | _ -> parse_expr tokens
  in
  let tokens = expect Semicolon tokens in

  (ReturnStmt expr, tokens)

and parse_expr_stmt tokens =
  let expr, tokens =
    match current_kind tokens with
    | Semicolon -> (EmptyExpr, tokens)
    | _ -> parse_expr tokens
  in
  let tokens = expect Semicolon tokens in

  (ExprStmt expr, tokens)

(* Declarations *)

and parse_declarator tokens =
  let id, tokens = parse_ident tokens in
  match current_kind tokens with
  | LBracket ->
      let tokens = expect LBracket tokens in
      let size, tokens =
        match current_kind tokens with
        | IntLit -> parse_int_lit tokens
        | _ -> (EmptyExpr, tokens)
      in
      let tokens = expect RBracket tokens in
      ((id, Some size), tokens)
  | _ -> ((id, None), tokens)

and parse_initialiser tokens =
  match current_kind tokens with
  | LCurly ->
      let tokens = expect LCurly tokens in

      let rec collect_exprs tokens =
        let expr, tokens = parse_expr tokens in
        match current_kind tokens with
        | Comma ->
            let tokens = expect Comma tokens in
            let rest, tokens = collect_exprs tokens in
            (expr :: rest, tokens)
        | _ -> (expr :: [], tokens)
      in

      let exprs, tokens = collect_exprs tokens in
      let tokens = expect RCurly tokens in
      (ArrayInitExpr exprs, tokens)
  | _ -> parse_expr tokens

and parse_init_declarator tokens =
  let (id, array_size), tokens = parse_declarator tokens in

  let init_expr, tokens =
    match current_kind tokens with
    | Eq ->
        let tokens = expect Eq tokens in
        parse_initialiser tokens
    | _ -> (EmptyExpr, tokens)
  in

  ((id, array_size, init_expr), tokens)

and parse_init_declarator_list tokens =
  let bundle, tokens = parse_init_declarator tokens in
  match current_kind tokens with
  | Comma ->
      let tokens = expect Comma tokens in
      let rest, tokens = parse_init_declarator_list tokens in
      (bundle :: rest, tokens)
  | _ -> (bundle :: [], tokens)

and parse_local_decl_list tokens =
  match current_kind tokens with
  | Void | Int | Float | Boolean ->
      let decls, tokens = parse_local_decl tokens in
      let rest, tokens = parse_local_decl_list tokens in
      (decls @ rest, tokens)
  | _ -> ([], tokens)

and parse_local_decl tokens =
  let base_type, tokens = parse_type tokens in
  let bundles, tokens = parse_init_declarator_list tokens in
  let tokens = expect Semicolon tokens in

  let decls =
    List.map
      (fun (id, array_size, init_expr) ->
        let final_type =
          match array_size with
          | Some size -> ArrayType (base_type, size)
          | None -> base_type
        in
        LocalVarDecl (final_type, id, init_expr))
      bundles
  in

  (decls, tokens)

let rec parse_global_decl tokens =
  let base_type, tokens = parse_type tokens in
  let id, tokens = parse_ident tokens in

  match current_kind tokens with
  | LParen ->
      (* it is a global function declaration *)
      let tokens = expect LParen tokens in
      let para_list, tokens = parse_para_list tokens in
      let body, tokens = parse_compound_stmt tokens in
      (FuncDecl (base_type, id, para_list, body) :: [], tokens)
  | _ ->
      (* it is a global variable declaration *)
      let array_size, tokens =
        match current_kind tokens with
        | LBracket ->
            let tokens = expect LBracket tokens in
            let size, tokens =
              match current_kind tokens with
              | IntLit -> parse_int_lit tokens
              | _ -> (EmptyExpr, tokens)
            in
            let tokens = expect RBracket tokens in
            (Some size, tokens)
        | _ -> (None, tokens)
      in

      let init_expr, tokens =
        match current_kind tokens with
        | Eq ->
            let tokens = expect Eq tokens in
            parse_initialiser tokens
        | _ -> (EmptyExpr, tokens)
      in

      let first_bundle = (id, array_size, init_expr) in
      let trailing_bundles, tokens =
        match current_kind tokens with
        | Comma ->
            let tokens = expect Comma tokens in
            parse_init_declarator_list tokens
        | _ -> ([], tokens)
      in
      let tokens = expect Semicolon tokens in

      let all_bundles = first_bundle :: trailing_bundles in
      let decls =
        List.map
          (fun (id, array, init_expr) ->
            let final_type =
              match array with
              | Some size -> ArrayType (base_type, size)
              | None -> base_type
            in
            GlobalVarDecl (final_type, id, init_expr))
          all_bundles
      in

      (decls, tokens)

(* Program *)

let rec parse_program tokens =
  match current_kind tokens with
  | EOF -> ([], tokens)
  | Void | Int | Float | Boolean ->
      let decls, tokens = parse_global_decl tokens in
      let rest, tokens = parse_program tokens in
      (decls @ rest, tokens)
  | _ ->
      parse_error
        ("\"" ^ (current_token tokens).spelling ^ "\" is not a valid start")
        tokens
