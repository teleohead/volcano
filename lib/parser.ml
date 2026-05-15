open Token
open Ast

type tokens = Token.t list

(* Helper Functions *)

let current_kind = function token :: _ -> token.kind | [] -> EOF

let current_token = function
  | token :: _ -> token
  | [] -> make EOF "$" Source_position.dummy

let advance = function _ :: tail -> tail | [] -> []

let parse_error msg tokens =
  Errors.report (Compile_error.make msg (current_token tokens).position);
  failwith "parse error"

let expect kind tokens =
  match current_kind tokens with
  | k when k = kind -> advance tokens
  | _ -> parse_error ("\"" ^ string_of_kind kind ^ "\" expected here") tokens

(* Base Cases *)

let parse_type tokens =
  match current_kind tokens with
  | Int -> (IntType, advance tokens)
  | Float -> (FloatType, advance tokens)
  | Boolean -> (BoolType, advance tokens)
  | Void -> (VoidType, advance tokens)
  | _ -> parse_error "type expected here" tokens

let parse_ident tokens =
  match tokens with
  | token :: tail when token.kind = Id -> (Ident token.spelling, tail)
  | _ -> parse_error "identifier expected here" tokens

let parse_operator tokens =
  let tok = current_token tokens in
  (Operator tok.spelling, advance tokens)

let parse_int_lit tokens =
  match tokens with
  | token :: tail when token.kind = IntLit -> (IntExpr token.spelling, tail)
  | _ -> parse_error "integer literal expected here" tokens

let parse_float_lit tokens =
  match tokens with
  | token :: tail when token.kind = FloatLit -> (FloatExpr token.spelling, tail)
  | _ -> parse_error "float literal expected here" tokens

let parse_bool_lit tokens =
  match tokens with
  | token :: tail when token.kind = BoolLit -> (BoolExpr token.spelling, tail)
  | _ -> parse_error "boolean literal expected here" tokens

let parse_string_lit tokens =
  match tokens with
  | token :: tail when token.kind = StringLit ->
      (StringExpr token.spelling, tail)
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
    | Lt | Gt | LtEq | GtEq ->
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
    | Lt | Gt | LtEq | GtEq ->
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
  let tokens = expect LParen tokens in
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
      let proper_list, tokens = parse_proper_arg_list tokens in
      (arg :: proper_list, tokens)
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
      let proper_list, tokens = parse_proper_para_list tokens in
      (para :: proper_list, tokens)
  | _ -> (para :: [], tokens)

let parse_para_list tokens =
  let tokens = expect LParen tokens in
  match current_kind tokens with
  | RParen -> ([], expect RParen tokens)
  | _ ->
      let proper_list, tokens = parse_proper_para_list tokens in
      let tokens = expect RParen tokens in
      (proper_list, tokens)
