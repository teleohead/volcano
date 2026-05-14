type state = {
  offset: int;
  ln: int;
  col: int;
  ch: char;
}

let init src = {
  offset = 0;
  ln = 1;
  col = 1;
  ch = Source_file.get src 0
}

let advance src s =
  let next_offset = s.offset + 1 in
  let next_ch = Source_file.get src next_offset in
  let (next_ln, next_col) = match s.ch with
    | '\n' | '\r' -> (s.ln + 1, 1)
    | '\t' -> (s.ln, s.col + 8 - (s.col - 1) mod 8)
    | _ -> (s.ln, s.col + 1)
  in
  { offset = next_offset; ln = next_ln; col = next_col; ch = next_ch}

let peek src s = Source_file.get src (s.offset + 1)
let peek2 src s = Source_file.get src (s.offset + 2)

let emit src kind spelling start_state end_state =
  let pos = Source_position.make
    start_state.ln end_state.ln
    start_state.col end_state.col
  in
  (Token.make kind spelling pos, advance src end_state)

let rec munch src s acc pred =
  let next = peek src s in
  if pred next then
    let s' = advance src s in
    munch src s' (acc ^ String.make 1 next) pred
  else
    (acc, s)
  
let rec skip_whitespace src s = failwith "TODO"

and skip_eol_comment src s = failwith "TODO"

and skip_block_comment src start s = failwith "TODO"

let scan_symbol src s = failwith "TODO"

let scan_ident src s = failwith "TODO"

let scan_number src s = failwith "TODO"

let scan_string src s = failwith "TODO"

let get_token src s =
  let s = skip_whitespace src s in
  match s.ch with
  | '\x00' -> emit src Token.EOF "$" s s
  | '"' -> scan_string src s
  | c when c >= '0' && c <= '9' -> scan_number src s
  | '.' when peek src s >= '0' && peek src s <= '9' -> scan_number src s
  | c when (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z') || c = '_' -> scan_ident src s
  | '+' | '-' | '*' | ';' | ','
  | '{' | '}' | '(' | ')' | '[' | ']'
  | '=' | '!' | '<' | '>' | '&' | '|' | '/' -> scan_symbol src s
  | c -> emit src Token.Error (String.make 1 c) s s
