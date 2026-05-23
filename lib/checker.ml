open Ast
open D_ast

(* The Symbol Table *)

type id_entry = d_decl

module SymbolTable = Map.Make (String)

type env = id_entry SymbolTable.t

let empty_env : env = SymbolTable.empty
let lookup id env : d_decl option = SymbolTable.find_opt id env

(* Helper Functions *)
let coerce_to_float e =
  match e.de_type with
  | FloatType -> e
  | _ -> { de_kind = D_ImplicitCastExpr e; de_type = FloatType }

let is_numeral_type t = match t with IntType | FloatType -> true | _ -> false

let declare_variable env id d_node =
  match SymbolTable.find_opt id env with
  | Some _ ->
      print_endline ("*2: identifier redeclared: " ^ id);
      env
  | None -> SymbolTable.add id d_node env
