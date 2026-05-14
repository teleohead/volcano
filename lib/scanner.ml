type state = { offset : int; ln : int; col : int; ch : char }

let init src = { offset = 0; ln = 1; col = 1; ch = Source_file.get src 0 }

let advance src s =
  let next_offset = s.offset + 1 in
  let next_ch = Source_file.get src next_offset in
  let next_ln, next_col =
    match s.ch with
    | '\n' | '\r' -> (s.ln + 1, 1)
    | '\t' -> (s.ln, s.col + 8 - ((s.col - 1) mod 8))
    | _ -> (s.ln, s.col + 1)
  in
  { offset = next_offset; ln = next_ln; col = next_col; ch = next_ch }

let peek src s = Source_file.get src (s.offset + 1)
let peek_two src s = Source_file.get src (s.offset + 2)

let emit src kind spelling start_state end_state =
  let pos =
    Source_position.make start_state.ln end_state.ln start_state.col
      end_state.col
  in
  (Token.make kind spelling pos, advance src end_state)

let rec munch src s acc pred =
  let next = peek src s in
  if pred next then
    let s' = advance src s in
    munch src s' (acc ^ String.make 1 next) pred
  else (acc, s)

let rec skip_whitespace src s =
  match s.ch with
  | ' ' | '\t' | '\x0c' | '\r' | '\n' -> skip_whitespace src (advance src s)
  | '/' when peek src s = '/' ->
      skip_whitespace src (skip_eol_comment src (advance src (advance src s)))
  | '/' when peek src s = '*' ->
      skip_whitespace src
        (skip_block_comment src s (advance src (advance src s)))
  | _ -> s

and skip_eol_comment src s =
  match s.ch with
  | '\n' | '\r' -> advance src s
  | c when c = Source_file.eof -> s
  | _ -> skip_eol_comment src (advance src s)

and skip_block_comment src start s =
  match s.ch with
  | c when c = Source_file.eof ->
      Errors.report
        (Compile_error.make "unterminated comment"
           (Source_position.make start.ln start.ln start.col start.col));
      s
  | '*' when peek src s = '/' -> advance src (advance src s)
  | _ -> skip_block_comment src start (advance src s)

let scan_symbol src s =
  match (s.ch, peek src s) with
  | '=', '=' -> emit src Token.EqEq "==" s (advance src s)
  | '!', '=' -> emit src Token.NotEq "!=" s (advance src s)
  | '<', '=' -> emit src Token.LtEq "<=" s (advance src s)
  | '>', '=' -> emit src Token.GtEq ">=" s (advance src s)
  | '&', '&' -> emit src Token.AndAnd "&&" s (advance src s)
  | '|', '|' -> emit src Token.OrOr "||" s (advance src s)
  | '=', _ -> emit src Token.Eq "=" s s
  | '!', _ -> emit src Token.Not "!" s s
  | '<', _ -> emit src Token.Lt "<" s s
  | '>', _ -> emit src Token.Gt ">" s s
  | '(', _ -> emit src Token.LParen "(" s s
  | ')', _ -> emit src Token.RParen ")" s s
  | '{', _ -> emit src Token.LCurly "{" s s
  | '}', _ -> emit src Token.RCurly "}" s s
  | '[', _ -> emit src Token.LBracket "[" s s
  | ']', _ -> emit src Token.RBracket "]" s s
  | ';', _ -> emit src Token.Semicolon ";" s s
  | ',', _ -> emit src Token.Comma "," s s
  | '+', _ -> emit src Token.Plus "+" s s
  | '-', _ -> emit src Token.Minus "-" s s
  | '*', _ -> emit src Token.Mult "*" s s
  | '/', _ -> emit src Token.Div "/" s s
  | c, _ -> emit src Token.Error (String.make 1 c) s s

let scan_ident src s =
  let is_ident_char c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '_'
  in
  let spelling, s' = munch src s (String.make 1 s.ch) is_ident_char in
  let kind = Token.kind_of_ident spelling in
  emit src kind spelling s s'

let scan_number src s =
  let is_digit c = c >= '0' && c <= '9' in

  let int_lex, int_s =
    if s.ch = '.' then ("", s) else munch src s (String.make 1 s.ch) is_digit
  in

  let frac_lex, frac_s, is_float =
    match peek src int_s with
    | '.' when s.ch <> '.' ->
        let dot_s = advance src int_s in
        let lex, s' = munch src dot_s "" is_digit in
        ("." ^ lex, s', true)
    | _ when s.ch = '.' ->
        let lex, s' = munch src int_s "" is_digit in
        ("." ^ lex, s', true)
    | _ -> ("", int_s, false)
  in

  let exp_lex, exp_s, has_exp =
    match peek src frac_s with
    | 'e' | 'E' ->
        let second = peek_two src frac_s in
        let third = Source_file.get src (frac_s.offset + 3) in
        let valid_unsigned = is_digit second in
        let valid_signed = (second = '+' || second = '-') && is_digit third in
        if valid_unsigned || valid_signed then
          let e_s = advance src frac_s in
          if valid_signed then
            let sign_s = advance src e_s in
            let digits, s' = munch src sign_s "" is_digit in
            (String.make 1 e_s.ch ^ String.make 1 sign_s.ch ^ digits, s', true)
          else
            let digits, s' = munch src e_s "" is_digit in
            (String.make 1 e_s.ch ^ digits, s', true)
        else ("", frac_s, false)
    | _ -> ("", frac_s, false)
  in

  let spelling = int_lex ^ frac_lex ^ exp_lex in
  let kind = if is_float || has_exp then Token.FloatLit else Token.IntLit in
  emit src kind spelling s exp_s

let scan_string src s =
  let is_unterminated c = c = '\n' || c = '\r' || c = Source_file.eof in
  let rec loop curr acc =
    let next = peek src curr in
    if is_unterminated next then begin
      Errors.report
        (Compile_error.make "unterminated string"
           (Source_position.make s.ln s.ln s.col s.col));
      (acc, curr)
    end
    else if next = '"' then (acc, advance src curr)
    else
      let curr' = advance src curr in
      match curr'.ch with
      | '\\' ->
          let escaped = peek src curr' in
          if is_unterminated escaped then (acc ^ "\\", curr')
          else
            let translated, s_after =
              match escaped with
              | 'n' -> ("\n", advance src curr')
              | 't' -> ("\t", advance src curr')
              | 'r' -> ("\r", advance src curr')
              | 'b' -> ("\b", advance src curr')
              | 'f' -> ("\x0c", advance src curr')
              | '"' -> ("\"", advance src curr')
              | '\'' -> ("\'", advance src curr')
              | '\\' -> ("\\", advance src curr')
              | _ ->
                  Errors.report
                    (Compile_error.make
                       (Printf.sprintf "illegal escape '\\%c'" escaped)
                       (Source_position.make s.ln curr'.ln s.col curr'.col));
                  ("", advance src curr')
            in
            loop s_after (acc ^ translated)
      | c -> loop curr' (acc ^ String.make 1 c)
  in
  let lexeme, s' = loop s "" in
  emit src Token.StringLit lexeme s s'

let get_token src s =
  let s = skip_whitespace src s in
  match s.ch with
  | c when c = Source_file.eof -> emit src Token.EOF "$" s s
  | '"' -> scan_string src s
  | c when c >= '0' && c <= '9' -> scan_number src s
  | '.' when peek src s >= '0' && peek src s <= '9' -> scan_number src s
  | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
      scan_ident src s
  | '+' | '-' | '*' | ';' | ',' | '{' | '}' | '(' | ')' | '[' | ']' | '=' | '!'
  | '<' | '>' | '&' | '|' | '/' ->
      scan_symbol src s
  | c -> emit src Token.Error (String.make 1 c) s s
