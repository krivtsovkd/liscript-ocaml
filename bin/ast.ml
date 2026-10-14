open Enums

type symbol = string

let make_symbol (str : string) : symbol = "\x00" ^ str

let symbol_name (symbol : symbol) : string =
  String.sub symbol 1 (String.length symbol - 1)

type ast_data =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | List of ast list
  | Symbol of symbol
  | Op of op
  | Pr of pr
  | Sf of sf

and loc = int * int
and ast = ast_data * loc

let rec show_ast ((value, _) : ast) : string =
  match value with
  | Int i -> Int.to_string i
  | Float f -> Float.to_string f
  | Bool b -> ( match b with true -> "true" | false -> "false")
  | Str s -> "\"" ^ s ^ "\""
  | Symbol s -> symbol_name s
  | Op o -> string_of_op o
  | Pr p -> string_of_pr p
  | Sf s -> string_of_sf s
  | List l ->
      let rec aux acc lst =
        match lst with
        | [] -> acc
        | hd :: [] -> acc ^ show_ast hd
        | hd :: tl -> aux (acc ^ show_ast hd ^ " ") tl
      in
      "(" ^ aux "" l ^ ")"

(* TODO *)
let rec show_data (value : ast_data) : string = show_ast (value, (0, 0))

let show_type ((value, _) : ast) : string =
  match value with
  | Int _ -> "int"
  | Float _ -> "float"
  | Bool _ -> "bool"
  | Str _ -> "string"
  | List _ -> "list"
  | Symbol _ -> "symbol"
  | Op _ -> "op"
  | Pr _ -> "pr"
  | Sf _ -> "sf"
