type t = { message : string; position : Source_position.t }

let make message position = { message; position }

let to_string e =
  Printf.sprintf "ERROR: %s: %s"
    (Source_position.to_string e.position)
    e.message
