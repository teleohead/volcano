let eof = '\x00'

type t = { chars : char array; filename : string }

let load filename =
  try
    let ic = open_in filename in
    let n = in_channel_length ic in
    let chars = Array.init n (fun _ -> input_char ic) in
    close_in ic;
    { chars = Array.append chars [| eof |]; filename }
  with Sys_error msg ->
    Printf.eprintf "[volcano]: cannot read: %s\n" msg;
    exit 1

let get src offset =
  if offset >= 0 && offset < Array.length src.chars then src.chars.(offset)
  else eof
