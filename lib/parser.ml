open Token
open Ast

type tokens = Token.t list

(* Helper Functions *)

let current_kind = function token :: _ -> token.kind | [] -> EOF

let current_token = function
  | token :: _ -> token
  | [] -> make EOF "$" Source_position.dummy

let advance = function _ :: rest -> rest | [] -> []

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
  | token :: rest when token.kind = Id -> (Ident token.spelling, rest)
  | _ -> parse_error "identifier expected here" tokens

let parse_operator tokens =
  let tok = current_token tokens in
  (Operator tok.spelling, advance tokens)

let parse_int_lit tokens =
  match tokens with
  | token :: rest when token.kind = IntLit -> (IntExpr token.spelling, rest)
  | _ -> parse_error "integer literal expected here" tokens

let parse_float_lit tokens =
  match tokens with
  | token :: rest when token.kind = FloatLit -> (FloatExpr token.spelling, rest)
  | _ -> parse_error "float literal expected here" tokens

let parse_bool_lit tokens =
  match tokens with
  | token :: rest when token.kind = BoolLit -> (BoolExpr token.spelling, rest)
  | _ -> parse_error "boolean literal expected here" tokens

let parse_string_lit tokens =
  match tokens with
  | token :: rest when token.kind = StringLit ->
      (StringExpr token.spelling, rest)
  | _ -> parse_error "string literal expected here" tokens

(* Expressions *)

let rec parse_expr tokens = parse_assignment_expr tokens

and parse_assignment_expr tokens =
  let lhs, rest = parse_cond_or_expr tokens in
  match current_kind rest with
  | Eq ->
      let rest = expect Eq rest in
      let rhs, rest = parse_assignment_expr rest in
      (AssignExpr (lhs, rhs), rest)
  | _ -> (lhs, rest)

and parse_cond_or_expr tokens =
  let init, rest = parse_cond_and_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | OrOr ->
        let op, rest = parse_operator tokens in
        let rhs, rest = parse_cond_and_expr rest in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc rest
    | _ -> (acc, tokens)
  in
  loop init rest

and parse_cond_and_expr tokens =
  let init, rest = parse_equality_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | AndAnd ->
        let op, rest = parse_operator tokens in
        let rhs, rest = parse_equality_expr rest in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc rest
    | _ -> (acc, tokens)
  in
  loop init rest

and parse_equality_expr tokens =
  let init, rest = parse_rel_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | EqEq | NotEq ->
        let op, rest = parse_operator tokens in
        let rhs, rest = parse_rel_expr rest in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc rest
    | _ -> (acc, tokens)
  in
  loop init rest

and parse_rel_expr tokens =
  let init, rest = parse_additive_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | Lt | Gt | LtEq | GtEq ->
        let op, rest = parse_operator tokens in
        let rhs, rest = parse_additive_expr rest in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc rest
    | _ -> (acc, tokens)
  in
  loop init rest

and parse_additive_expr tokens =
  let init, rest = parse_multiplicative_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | Lt | Gt | LtEq | GtEq ->
        let op, rest = parse_operator tokens in
        let rhs, rest = parse_multiplicative_expr rest in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc rest
    | _ -> (acc, tokens)
  in
  loop init rest

and parse_multiplicative_expr tokens =
  let init, rest = parse_unary_expr tokens in
  let rec loop acc tokens =
    match current_kind tokens with
    | Lt | Gt | LtEq | GtEq ->
        let op, rest = parse_operator tokens in
        let rhs, rest = parse_unary_expr rest in
        let acc = BinaryExpr (acc, op, rhs) in
        loop acc rest
    | _ -> (acc, tokens)
  in
  loop init rest

and parse_unary_expr tokens =
  match current_kind tokens with
  | Plus | Minus | Not ->
      let op, rest = parse_operator tokens in
      let operand, rest = parse_unary_expr rest in
      (UnaryExpr (op, operand), rest)
  | _ -> parse_primary_expr tokens

and parse_primary_expr tokens =
  match current_kind tokens with
  | Id -> (
      let id, rest = parse_ident tokens in
      match current_kind rest with
      | LParen ->
          let rest = expect LParen rest in
          let args, rest = parse_arg_list rest in
          (CallExpr (id, args), rest)
      | LBracket ->
          let rest = expect LBracket rest in
          let index, rest = parse_expr rest in
          let rest = expect RBracket rest in
          (ArrayExpr (SimpleVar id, index), rest)
      | _ -> (VarExpr (SimpleVar id), rest))
  | LParen ->
      let rest = expect LParen tokens in
      let expr, rest = parse_expr rest in
      let rest = expect RParen tokens in
      (expr, rest)
  | IntLit -> parse_int_lit tokens
  | FloatLit -> parse_float_lit tokens
  | BoolLit -> parse_bool_lit tokens
  | StringLit -> parse_string_lit tokens
  | _ ->
      parse_error ("\"" ^ string_of_kind Semicolon ^ "\" expected here") tokens

(* Arguments *)

let parse_arg_list tokens = ()
