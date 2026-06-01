let scan src =
  let rec collect_tokens s =
    let tok, s' = Scanner.get_token src s in
    match tok.Token.kind with
    | Token.EOF -> tok :: []
    | _ -> tok :: collect_tokens s'
  in
  collect_tokens (Scanner.init src)

let parse tokens = match Parser.parse_program tokens with ast, _ -> ast

let compile_source src =
  try
    let tokens = scan src in
    let ast = parse tokens in
    (* TODO: Checker / Emitter *)
    let unparsed_code = Unparser.unparse_program ast in
    Printf.printf "Compilation was successful.\n%s" unparsed_code
  with Failure msg -> Printf.printf "ERROR: %s\n" msg
