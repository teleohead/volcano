open Volcano

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Please provide the source file as an argument.\n";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let src = Source_file.load filename in

  Printf.printf "======= The VC compiler =======\n";

  let rec collect_tokens s =
    let tok, s' = Scanner.get_token src s in
    if tok.Token.kind = Token.EOF then [ tok ] else tok :: collect_tokens s'
  in
  let tokens = collect_tokens (Scanner.init src) in

  try
    let ast, _ = Parser.parse_program tokens in
    let unparsed_code = Unparser.unparse_program ast in
    Printf.printf "Compilation was successful.\n%s" unparsed_code
  with Failure msg -> Printf.printf "ERROR: %s\n" msg
