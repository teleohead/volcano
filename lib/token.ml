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

type t = { kind : kind; spelling : string; position : Source_position.t }

let keyword_map =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [
      ("boolean", Boolean);
      ("break", Break);
      ("continue", Continue);
      ("else", Else);
      ("float", Float);
      ("for", For);
      ("if", If);
      ("int", Int);
      ("return", Return);
      ("void", Void);
      ("while", While);
      ("true", BoolLit);
      ("false", BoolLit);
    ];
  tbl

let kind_of_ident spelling =
  match Hashtbl.find_opt keyword_map spelling with Some k -> k | None -> Id

let kind_to_int = function
  | Boolean -> 0
  | Break -> 1
  | Continue -> 2
  | Else -> 3
  | Float -> 4
  | For -> 5
  | If -> 6
  | Int -> 7
  | Return -> 8
  | Void -> 9
  | While -> 10
  | Plus -> 11
  | Minus -> 12
  | Mult -> 13
  | Div -> 14
  | Not -> 15
  | NotEq -> 16
  | Eq -> 17
  | EqEq -> 18
  | Lt -> 19
  | LtEq -> 20
  | Gt -> 21
  | GtEq -> 22
  | AndAnd -> 23
  | OrOr -> 24
  | LCurly -> 25
  | RCurly -> 26
  | LParen -> 27
  | RParen -> 28
  | LBracket -> 29
  | RBracket -> 30
  | Semicolon -> 31
  | Comma -> 32
  | Id -> 33
  | IntLit -> 34
  | FloatLit -> 35
  | BoolLit -> 36
  | StringLit -> 37
  | Error -> 38
  | EOF -> 39

let make kind spelling position = { kind; spelling; position }

let string_of_kind = function
  | Boolean -> "boolean"
  | Break -> "break"
  | Continue -> "continue"
  | Else -> "else"
  | Float -> "float"
  | For -> "for"
  | If -> "if"
  | Int -> "int"
  | Return -> "return"
  | Void -> "void"
  | While -> "while"
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Not -> "!"
  | NotEq -> "!="
  | Eq -> "="
  | EqEq -> "=="
  | Lt -> "<"
  | LtEq -> "<="
  | Gt -> ">"
  | GtEq -> ">="
  | AndAnd -> "&&"
  | OrOr -> "||"
  | LCurly -> "{"
  | RCurly -> "}"
  | LParen -> "("
  | RParen -> ")"
  | LBracket -> "["
  | RBracket -> "]"
  | Semicolon -> ";"
  | Comma -> ","
  | Id -> "<id>"
  | IntLit -> "<int-literal>"
  | FloatLit -> "<float-literal>"
  | BoolLit -> "<boolean-literal>"
  | StringLit -> "<string-literal>"
  | Error -> "<error>"
  | EOF -> "$"

let to_string t =
  Printf.sprintf "Kind = %d [%s], spelling = \"%s\", position = %s"
    (kind_to_int t.kind) (string_of_kind t.kind) t.spelling
    (Source_position.to_string t.position)
