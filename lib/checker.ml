open Ast

(* The Symbol Table *)

type id_entry = decl

module SymbolTable = Map.Make (String)

type env = id_entry SymbolTable.t

let is_declared_locally id env = SymbolTable.mem id env
let lookup id env = SymbolTable.find_opt id env
