open Enums

module Symbol = struct
  type t = string

  let make (str : string) : t = "\x00" ^ str
  let name (sym : t) : string = String.sub sym 1 (String.length sym - 1)
end

module StringMap = Map.Make (String)

(* (start, end, filename) *)
type loc = int * int * string

exception EvalError of (string * loc)

type ast_data =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | List of ast list
  | Symbol of Symbol.t
  | Op of op
  | Pr of pr
  | Sf of sf
  | Lambda of lambda

and ast = ast_data * loc
and envt = ast_data StringMap.t
and lambda = { args : ast list; body : ast; get_env : envt -> envt }

let env_def (env : envt) (sym : Symbol.t) (value : ast_data) : envt =
  StringMap.add (Symbol.name sym) value env

let env_get (env : envt) ((sym, lc) : ast) : ast_data =
  match sym with
  | Symbol s -> (
      match StringMap.find_opt (Symbol.name s) env with
      | Some x -> x
      | None ->
          if Settings.auto_quoting then
            sym
          else
            raise @@ EvalError ("Undefined variable", lc))
  | _ -> raise @@ EvalError ("Expected symbol", lc)

let env_set (env : envt) (sym : Symbol.t) (value : ast_data) : envt =
  StringMap.add (Symbol.name sym) value env

let make_lambda (args : ast list) (body : ast) (env : envt) : lambda =
  { args; body; get_env = (fun _ -> env) }

let id_env (env : envt) : envt = env

let make_dambda (args : ast list) (body : ast) : lambda =
  { args; body; get_env = id_env }

let make_dambda (args : ast list) (body : ast) (_ : envt) : lambda =
  { args; body; get_env = id_env }

let is_dambda (l : lambda) : bool = l.get_env == id_env

let rec show_data (value : ast_data) : string =
  match value with
  | Int i -> Int.to_string i
  | Float f -> Float.to_string f
  | Bool b -> ( match b with true -> "true" | false -> "false")
  | Str s -> "\"" ^ s ^ "\""
  | Symbol s -> Symbol.name s
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
