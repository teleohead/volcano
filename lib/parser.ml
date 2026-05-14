type tokens = Token.t list

let current = function tok :: _ -> tok.Token.kind | [] -> Token.EOF

let current_tok = function
  | tok :: _ -> tok
  | [] -> Token.make Token.EOF "$" Source_position.dummy

let advance = function _ :: rest -> rest | [] -> []

let expect kind tokens =
  match tokens with
  | tok :: rest when tok.Token.kind = kind -> rest
  | tok :: _ ->
      Errors.report
        (Compile_error.make
           (Printf.sprintf "\"%s\" expected here" (Token.string_of_kind kind))
           tok.Token.position);
      tokens
  | [] ->
      Errors.report
        (Compile_error.make
           (Printf.sprintf "\"%s\" expected here" (Token.string_of_kind kind))
           Source_position.dummy);
      []
