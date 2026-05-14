let diagnostics : Compile_error.t list ref = ref []
let report err = diagnostics := err :: !diagnostics
let get () = List.rev !diagnostics
let has_errors () = !diagnostics <> []
let clear () = diagnostics := []
