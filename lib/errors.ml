type semantic_kind =
  | MISSING_MAIN
  | MAIN_RETURN_TYPE_NOT_INT
  | IDENTIFIER_REDECLARED of string
  | IDENTIFIER_DECLARED_VOID of string
  | IDENTIFIER_DECLARED_VOID_ARRAY of string
  | IDENTIFIER_UNDECLARED of string
  | INCOMPATIBLE_TYPE_FOR_ASSIGNMENT
  | INVALID_LVALUE_IN_ASSIGNMENT
  | INCOMPATIBLE_TYPE_FOR_RETURN
  | INCOMPATIBLE_TYPE_FOR_BINARY_OPERATOR
  | INCOMPATIBLE_TYPE_FOR_UNARY_OPERATOR
  | ARRAY_FUNCTION_AS_SCALAR
  | SCALAR_FUNCTION_AS_ARRAY
  | WRONG_TYPE_FOR_ARRAY_INITIALISER
  | INVALID_INITIALISER_ARRAY_FOR_SCALAR
  | INVALID_INITIALISER_SCALAR_FOR_ARRAY
  | EXCESS_ELEMENTS_IN_ARRAY_INITIALISER
  | ARRAY_SUBSCRIPT_NOT_INTEGER
  | ARRAY_SIZE_MISSING
  | SCALAR_ARRAY_AS_FUNCTION
  | IF_CONDITIONAL_NOT_BOOLEAN of string
  | FOR_CONDITIONAL_NOT_BOOLEAN of string
  | WHILE_CONDITIONAL_NOT_BOOLEAN of string
  | BREAK_NOT_IN_LOOP
  | CONTINUE_NOT_IN_LOOP
  | TOO_MANY_ACTUAL_PARAMETERS
  | TOO_FEW_ACTUAL_PARAMETERS
  | WRONG_TYPE_FOR_ACTUAL_PARAMETER
  | STATEMENTS_NOT_REACHED
  | MISSING_RETURN_STATEMENT

let string_of_semantic = function
  | MISSING_MAIN -> "*0: main function is missing"
  | MAIN_RETURN_TYPE_NOT_INT -> "*1: return type of main is not int"
  | IDENTIFIER_REDECLARED id -> "*2: identifier redeclared: " ^ id
  | IDENTIFIER_DECLARED_VOID id -> "*3: identifier declared void: " ^ id
  | IDENTIFIER_DECLARED_VOID_ARRAY id -> "*4: identifier declared void[]: " ^ id
  | IDENTIFIER_UNDECLARED id -> "*5: identifier undeclared: " ^ id
  | INCOMPATIBLE_TYPE_FOR_ASSIGNMENT -> "*6: incompatible type for ="
  | INVALID_LVALUE_IN_ASSIGNMENT -> "*7: invalid lvalue in assignment"
  | INCOMPATIBLE_TYPE_FOR_RETURN -> "*8: incompatible type for return"
  | INCOMPATIBLE_TYPE_FOR_BINARY_OPERATOR ->
      "*9: incompatible type for this binary operator"
  | INCOMPATIBLE_TYPE_FOR_UNARY_OPERATOR ->
      "*10: incompatible type for this unary operator"
  | ARRAY_FUNCTION_AS_SCALAR ->
      "*11: attempt to use an array/function as a scalar"
  | SCALAR_FUNCTION_AS_ARRAY ->
      "*12: attempt to use a scalar/function as an array"
  | WRONG_TYPE_FOR_ARRAY_INITIALISER ->
      "*13: wrong type for element in array initialiser"
  | INVALID_INITIALISER_ARRAY_FOR_SCALAR ->
      "*14: invalid initialiser: array initialiser for scalar"
  | INVALID_INITIALISER_SCALAR_FOR_ARRAY ->
      "*15: invalid initialiser: scalar initialiser for array"
  | EXCESS_ELEMENTS_IN_ARRAY_INITIALISER ->
      "*16: excess elements in array initialiser"
  | ARRAY_SUBSCRIPT_NOT_INTEGER -> "*17: array subscript is not an integer"
  | ARRAY_SIZE_MISSING -> "*18: array size missing"
  | SCALAR_ARRAY_AS_FUNCTION ->
      "*19: attempt to reference a scalar/array as a function"
  | IF_CONDITIONAL_NOT_BOOLEAN t ->
      Printf.sprintf "*20: if conditional is not boolean (found: %s)" t
  | FOR_CONDITIONAL_NOT_BOOLEAN t ->
      Printf.sprintf "*21: for conditional is not boolean (found: %s)" t
  | WHILE_CONDITIONAL_NOT_BOOLEAN t ->
      Printf.sprintf "*22: while conditional is not boolean (found: %s)" t
  | BREAK_NOT_IN_LOOP -> "*23: break must be in a while/for"
  | CONTINUE_NOT_IN_LOOP -> "*24: continue must be in a while/for"
  | TOO_MANY_ACTUAL_PARAMETERS -> "*25: too many actual parameters"
  | TOO_FEW_ACTUAL_PARAMETERS -> "*26: too few actual parameters"
  | WRONG_TYPE_FOR_ACTUAL_PARAMETER -> "*27: wrong type for actual parameter"
  | STATEMENTS_NOT_REACHED -> "*30: statement(s) not reached"
  | MISSING_RETURN_STATEMENT -> "*31: missing return statement"

let report_lexical pos msg =
  Printf.printf "ERROR: %s: Lexical error: %s\n"
    (Source_position.to_string pos)
    msg

let report_syntax (msg : string) = Printf.printf "ERROR: %s\n" msg

let report_semantic pos token sem_kind =
  let msg = string_of_semantic sem_kind in
  Printf.printf "ERROR: %s: %s: %s\n" (Source_position.to_string pos) token msg
