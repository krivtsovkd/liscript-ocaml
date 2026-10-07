type symbol = string

let make_symbol (str : string) : symbol = "\x00" ^ str

let symbol_name (symbol : symbol) : string =
  String.sub symbol 1 (String.length symbol - 1)

type ast =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | List of ast list
  | Symbol of symbol

let rec show (value : ast) : string =
  match value with
  | Int i -> Int.to_string i
  | Float f -> Float.to_string f
  | Bool b -> ( match b with true -> "true" | false -> "false")
  | Str s -> "\"" ^ s ^ "\""
  | Symbol s -> symbol_name s
  | List l ->
      let rec aux acc lst =
        match lst with
        | [] -> acc
        | hd :: [] -> acc ^ show hd
        | hd :: tl -> aux (acc ^ show hd ^ " ") tl
      in
      "(" ^ aux "" l ^ ")"
