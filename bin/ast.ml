open Enums

type symbol = string

let make_symbol (str : string) : symbol = "\x00" ^ str

let symbol_name (symbol : symbol) : string =
  String.sub symbol 1 (String.length symbol - 1)

module StringMap = Map.Make (String)

type loc = int * int

exception EvalError of (string * loc)

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
  | Lambda of lambda

and ast = ast_data * loc
and lambda = { args : ast list; body : ast; get_env : envt -> envt }
and envt = ast_data StringMap.t

let env_def (env : envt) (sym : symbol) (value : ast_data) : envt =
  StringMap.add (symbol_name sym) value env

let env_get (env : envt) ((sym, lc) : ast) : ast_data =
  match sym with
  | Symbol s -> (
      match StringMap.find_opt (symbol_name s) env with
      | Some x -> x
      | None ->
          if Settings.auto_quoting then
            sym
          else
            raise @@ EvalError ("Undefined variable", lc))
  | _ -> raise @@ EvalError ("Expected symbol", lc)

let env_set (env : envt) (sym : symbol) (value : ast_data) : envt =
  StringMap.add (symbol_name sym) value env

let make_lambda (args : ast list) (body : ast) (env : envt) : lambda =
  { args; body; get_env = (fun _ -> env) }

let make_dambda (args : ast list) (body : ast) : lambda =
  { args; body; get_env = (fun env -> env) }

let make_dambda (args : ast list) (body : ast) (_ : envt) : lambda =
  { args; body; get_env = (fun env -> env) }

let rec show_data (value : ast_data) : string =
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
  | Lambda l ->
      "(Lambda or Dambda) " ^ show_data (List l.args) ^ " " ^ show_ast l.body

and show_ast ((value, _) : ast) : string = show_data value

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
  | Lambda _ -> "lambda"

let print_env (env : envt) =
  StringMap.bindings env
  |> List.iter (fun (name, value) ->
      print_endline @@ name ^ ": " ^ show_data value)
