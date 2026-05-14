type kind =
  (* Keywords *)
  | Boolean
  | Break
  | Continue
  | Else
  | Float
  | For
  | If
  | Int
  | Return
  | Void
  | While
  (* Operators *)
  | Plus
  | Minus
  | Mult
  | Div
  | Not
  | NotEq
  | Eq
  | EqEq
  | Lt
  | LtEq
  | Gt
  | GtEq
  | AndAnd
  | OrOr
  (* Separators *)
  | LCurly
  | RCurly
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Semicolon
  | Comma
  (* Identifier *)
  | Id
  (* Literals *)
  | IntLit
  | FloatLit
  | BoolLit
  | StringLit
  (* Special *)
  | Error
  | EOF

type t = {
  kind: kind;
  spelling: string;
  position: Source_position.t;
}

let keyword_map =
  let tbl = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) [
    "boolean",  Boolean;
    "break",    Break;
    "continue", Continue;
    "else",     Else;
    "float",    Float;
    "for",      For;
    "if",       If;
    "int",      Int;
    "return",   Return;
    "void",     Void;
    "while",    While;
    "true",     BoolLit;
    "false",    BoolLit;
  ];
  tbl

let kind_of_ident spelling =
  match Hashtbl.find_opt keyword_map spelling with
  | Some k -> k
  | None -> Id

let make kind spelling position =
  { kind; spelling; position }

let string_of_kind = function
  | Boolean   -> "boolean"
  | Break     -> "break"
  | Continue  -> "continue"
  | Else      -> "else"
  | Float     -> "float"
  | For       -> "for"
  | If        -> "if"
  | Int       -> "int"
  | Return    -> "return"
  | Void      -> "void"
  | While     -> "while"
  | Plus      -> "+"
  | Minus     -> "-"
  | Mult      -> "*"
  | Div       -> "/"
  | Not       -> "!"
  | NotEq     -> "!="
  | Eq        -> "="
  | EqEq      -> "=="
  | Lt        -> "<"
  | LtEq      -> "<="
  | Gt        -> ">"
  | GtEq      -> ">="
  | AndAnd    -> "&&"
  | OrOr      -> "||"
  | LCurly    -> "{"
  | RCurly    -> "}"
  | LParen    -> "("
  | RParen    -> ")"
  | LBracket  -> "["
  | RBracket  -> "]"
  | Semicolon -> ";"
  | Comma     -> ","
  | Id        -> "<id>"
  | IntLit    -> "<int-literal>"
  | FloatLit  -> "<float-literal>"
  | BoolLit   -> "<boolean-literal>"
  | StringLit -> "<string-literal>"
  | Error     -> "<error>"
  | EOF       -> "$"


let to_string t =
  Printf.sprintf "Kind = %s, spelling = \"%s\", position = %s"
  (string_of_kind t.kind)
  t.spelling
  (Source_position.to_string t.position)

