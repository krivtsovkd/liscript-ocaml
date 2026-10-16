open Ast

(* TODO *)
let explod str = List.init (String.length str) (String.get str)

exception Unreachable
exception EvalError of (string * loc)

module Env = struct
  module StringMap = Map.Make (String)

  type t = ast_data StringMap.t

  let def (env : t) (sym : symbol) (value : ast_data) : t =
    StringMap.add (symbol_name sym) value env

  let get (env : t) ((sym, lc) : ast) : ast_data =
    match sym with
    | Symbol s -> (
        try StringMap.find (symbol_name s) env
        with Not_found -> raise @@ EvalError ("Undefined variable", lc))
    | _ -> raise @@ EvalError ("Expected symbol", lc)

  let set (env : t) (sym : symbol) (value : ast_data) : t =
    StringMap.add (symbol_name sym) value env
end

let repr (v : ast_data) : string = match v with Str s -> s | _ -> show_data v
let repr ((v, _) : ast) : string = repr v

let op_int (o : Enums.op) (x : int) (y : int) : int =
  match o with
  | Concat -> raise Unreachable
  | Add -> x + y
  | Sub -> x - y
  | Mul -> x * y
  | Div -> x / y
  | Mod -> x mod y

let op_float (o : Enums.op) (x : float) (y : float) : float =
  match o with
  | Concat -> raise Unreachable
  | Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> x /. y
  | Mod -> Float.rem x y

let op_funs (o : Enums.op) (a : ast) (b : ast) (lc : loc) : ast =
  match o with
  | Concat -> (Str (repr a ^ repr b), lc)
  | Add | Sub | Mul | Div | Mod -> (
      match (a, b) with
      | (Int x, _), (Int y, _) -> (Int (op_int o x y), lc)
      | (Float x, _), (Float y, _) -> (Float (op_float o x y), lc)
      | _ ->
          raise
          @@ EvalError
               ( "Cant apply binary operation to " ^ show_ast a ^ " and "
                 ^ show_ast b,
                 lc ))

let pr_num (p : Enums.pr) (x : 'a) (y : 'a) : bool =
  match p with
  | Eq -> x = y
  | Gt -> x > y
  | Lt -> x < y
  | Gte -> x >= y
  | Lte -> x <= y

let pr_funs (p : Enums.pr) (a : ast) (b : ast) (lc : loc) : bool =
  match p with
  | Eq -> a = b
  | Gt | Lt | Gte | Lte -> (
      match (a, b) with
      | (Int x, _), (Int y, _) -> pr_num p x y
      | (Float x, _), (Float y, _) -> pr_num p x y
      | _ ->
          raise
          @@ EvalError
               ( "Cant apply predicate to " ^ show_ast a ^ " and " ^ show_ast b,
                 lc ))

let assert_length (args : ast list) (len : int) (lc : loc) =
  if List.length args <> len then
    raise
    @@ EvalError
         ( "Expected " ^ string_of_int len ^ " arguments, got " ^ string_of_int
           @@ List.length args,
           lc )

let rec fold_op (op : Enums.op) (values : ast list) (lc : loc) (env : Env.t) :
    ast * Env.t =
  match values with
  | [] -> raise @@ EvalError ("Operation on empty list", lc)
  | x :: xs ->
      let rec loop (acc : ast) (env : Env.t) (rest : ast list) : ast * Env.t =
        match rest with
        | [] -> (acc, env)
        | x :: xs ->
            let x, env = eval env x in
            loop (op_funs op acc x lc) env xs
      in
      let x, env = eval env x in
      loop x env xs

and fold_pr (p : Enums.pr) (values : ast list) (lc : loc) (env : Env.t) :
    ast * Env.t =
  let rec loop (vals : ast list) (env : Env.t) : ast_data * Env.t =
    match vals with
    | [] | [ _ ] -> (Bool true, env)
    | x :: y :: xs ->
        let ey, env = eval env y in
        if not (pr_funs p x ey lc) then
          (Bool false, env)
        else
          loop (ey :: xs) env
  in
  match values with
  | [] -> ((Bool true, lc), env)
  | x :: xs ->
      let x, env = eval env x in
      let res, env = loop (x :: xs) env in
      ((res, lc), env)

and handle_sf (s : Enums.sf) (tail : ast list) (lc : loc) (env : Env.t) :
    ast * Env.t =
  let _ =
    match Enums.sf_arg_count s with
    | Some cnt -> assert_length tail cnt lc
    | None -> ()
  in

  match s with
  | Quote -> ((fst @@ List.hd tail, lc), env)
  | Typeof ->
      let s, env = eval env @@ List.hd tail in
      ((Str (show_type s), lc), env)
  | Cons -> (
      let hd, env = eval env @@ List.hd tail in
      let arg2, env = eval env @@ List.hd @@ List.tl tail in
      match arg2 with
      | List tl, _ -> ((List (hd :: tl), lc), env)
      | _, lc2 ->
          raise @@ EvalError ("Expected cons list, got " ^ show_ast arg2, lc2))
  | Car -> (
      let lst, env = eval env @@ List.hd tail in
      match lst with
      | List l, lc2 -> (
          match l with
          | [] -> raise @@ EvalError ("Expected nonempty list", lc2)
          | (x, _) :: _ -> ((x, lc), env))
      | res, lc2 ->
          raise @@ EvalError ("Expected cons list, got " ^ show_data res, lc2))
  | Cdr -> (
      let lst, env = eval env @@ List.hd tail in
      match lst with
      | List l, lc2 -> (
          match l with
          | [] -> raise @@ EvalError ("Expected nonempty list", lc2)
          | (_, _) :: xs -> ((List xs, lc), env))
      | res, lc2 ->
          raise @@ EvalError ("Expected cons list, got " ^ show_data res, lc2))
  | Cond -> (
      if List.length tail mod 2 = 0 then
        raise
        @@ EvalError
             ( "Expected an odd number of arguments, got "
               ^ string_of_int (List.length tail),
               lc )
      else
        match tail with
        | [] -> raise Unreachable
        | x :: [] -> eval env x
        | x :: y :: xs -> (
            let x, env = eval env x in
            match x with
            | Bool cond, lc2 ->
                if cond then
                  eval env y
                else
                  handle_sf s xs lc2 env
            | res, lc2 ->
                raise @@ EvalError ("Expected bool, got " ^ show_data res, lc2))
      )
  | Print ->
      let rec loop (acc : string list) (rest : ast list) (env : Env.t) :
          string list * Env.t =
        match rest with
        | [] -> (acc, env)
        | x :: xs ->
            let x, env = eval env x in
            loop (repr x :: acc) xs env
      in
      let reprs, env = loop [] tail env in
      print_endline @@ String.concat " " reprs;
      ((List [], lc), env)
  | Read -> (
      (* TODO not copypaste print *)
      let rec loop (acc : string list) (rest : ast list) (env : Env.t) :
          string list * Env.t =
        match rest with
        | [] -> (acc, env)
        | x :: xs ->
            let x, env = eval env x in
            loop (repr x :: acc) xs env
      in
      let reprs, env = loop [] tail env in
      print_endline @@ String.concat " " reprs;

      match In_channel.input_line stdin with
      | None -> ((List [], lc), env)
      | Some line -> (
          match Parser.parse (explod line, 0) with
          | Failure (reason, _, _) ->
              raise @@ EvalError ("Input parse error:\n" ^ reason, lc)
          | Success (value, _) -> (value, env)))
  | Symbol ->
      let s, env = eval env @@ List.hd tail in
      ((Symbol (s |> repr |> make_symbol), lc), env)
  | Eval ->
      let x, env = eval env @@ List.hd tail in
      let x', env = eval env x in
      ((fst x', lc), env)
  | Do -> (
      match tail with
      | [] -> ((List [], lc), env)
      | x :: [] -> eval env x
      | x :: xs ->
          let _, env = eval env x in
          handle_sf s xs lc env)
  | Def | Set -> (
      let f =
        if s = Def then
          Env.def
        else
          Env.set
      in
      match tail with
      | [] -> ((List [], lc), env)
      | _ :: [] ->
          raise
          @@ EvalError
               ( "Expected an even number of values, got "
                 ^ string_of_int (List.length tail),
                 lc )
      | sym :: value :: xs -> (
          match sym with
          | Symbol sym, _ ->
              let (evalue, _), env = eval env value in
              let env = f env sym evalue in
              handle_sf s xs lc env
          | _, lc' ->
              raise @@ EvalError ("Expected symbol, got " ^ show_ast sym, lc')))

and eval_list (l : ast list) (lc : loc) (env : Env.t) : ast * Env.t =
  match l with
  | [] -> ((List [], lc), env)
  | unevaled_head :: tail -> (
      match eval env unevaled_head with
      | (Op o, _), env -> fold_op o tail lc env
      | (Pr p, _), env -> fold_pr p tail lc env
      | (Sf s, _), env -> handle_sf s tail lc env
      | _ -> raise @@ EvalError ("Head not callable", lc))

and eval (env : Env.t) ((value, lc) : ast) : ast * Env.t =
  match value with
  | Symbol _ -> ((Env.get env (value, lc), lc), env)
  | List l -> eval_list l lc env
  | _ -> ((value, lc), env)
