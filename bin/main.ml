open Volcano

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Please provide the source file as an argument.\n";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let src = Source_file.load filename in

  Printf.printf "======= The VC compiler =======\n";

  Compile.compile_source src
