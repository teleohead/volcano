type t = { message : string; token : string; position : Source_position.t }

let make ?(token = "") message position = { message; token; position }

let to_string e =
  if e.token = "" then
    Printf.sprintf "ERROR: %s: : %s"
      (Source_position.to_string e.position)
      e.message
  else
    Printf.sprintf "ERROR: %s: %s: %s"
      (Source_position.to_string e.position)
      e.token e.message
