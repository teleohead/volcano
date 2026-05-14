open Token

type tokens = Token.t list

let current = function token :: _ -> token.kind | [] -> EOF

let current_tok = function
  | tok :: _ -> tok
  | [] -> make EOF "$" Source_position.dummy

let advance = function _ :: rest -> rest | [] -> []

let parse_error msg tokens =
  Errors.report (Compile_error.make msg (current_tok tokens).position);
  failwith "parse error"

let expect kind tokens =
  match tokens with
  | token :: rest when token.kind = kind -> rest
  | _ -> parse_error ("\"" ^ string_of_kind kind ^ "\" expected here") tokens

let parse_type tokens =
  match current tokens with
  | Int -> (Ast.IntType, advance tokens)
  | Float -> (Ast.FloatType, advance tokens)
  | Boolean -> (Ast.BoolType, advance tokens)
  | Void -> (Ast.VoidType, advance tokens)
  | _ -> parse_error "type expected here" tokens

let parse_ident tokens =
  match tokens with
  | token :: rest when token.kind = Id -> (Ast.Ident token.spelling, rest)
  | _ -> parse_error "identifier expected here" tokens

let parse_operator tokens =
  let tok = current_tok tokens in
  (Ast.Operator tok.spelling, advance tokens)

let parse_int_lit tokens =
  match tokens with
  | token :: rest when token.kind = IntLit -> (Ast.IntExpr token.spelling, rest)
  | _ -> parse_error "integer literal expected here" tokens

let parse_float_lit tokens =
  match tokens with
  | token :: rest when token.kind = FloatLit ->
      (Ast.FloatExpr token.spelling, rest)
  | _ -> parse_error "float literal expected here" tokens

let parse_bool_lit tokens =
  match tokens with
  | token :: rest when token.kind = BoolLit ->
      (Ast.BoolExpr token.spelling, rest)
  | _ -> parse_error "boolean literal expected here" tokens

let parse_string_lit tokens =
  match tokens with
  | token :: rest when token.kind = StringLit ->
      (Ast.StringExpr token.spelling, rest)
  | _ -> parse_error "string literal expected here" tokens
