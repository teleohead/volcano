let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Please provide the source file as an argument.\n";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let src = Volcano.Source_file.load filename in
  Printf.printf "======= The VC compiler =======\n";
  let rec loop s =
    let tok, s' = Volcano.Scanner.get_token src s in
    Printf.printf "%s\n" (Volcano.Token.to_string tok);
    if tok.Volcano.Token.kind <> Volcano.Token.EOF then loop s'
  in
  loop (Volcano.Scanner.init src)
