type state

val init: Source_file.t -> state
val get_token: Source_file.t -> state -> Token.t * state
