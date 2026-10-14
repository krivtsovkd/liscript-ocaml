open Ast

(* TODO *)
let explod str = List.init (String.length str) (String.get str)

exception Unreachable
exception EvalError of (string * loc)

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

let rec fold_op (op : Enums.op) (values : ast list) (lc : loc) : ast =
  match values with
  | [] -> raise @@ EvalError ("Operation on empty list", lc)
  | x :: xs ->
      List.fold_left (fun acc v -> op_funs op acc (eval v) lc) (eval x) xs

and fold_pr (p : Enums.pr) (values : ast list) (lc : loc) : ast =
  let rec loop (vals : ast list) =
    match vals with
    | [] | [ _ ] -> true
    | x :: y :: xs ->
        let ey = eval y in
        if not (pr_funs p x ey lc) then
          false
        else
          loop (ey :: xs)
  in
  (Bool (match values with [] -> true | x :: xs -> loop (eval x :: xs)), lc)

and handle_sf (s : Enums.sf) (tail : ast list) (lc : loc) : ast =
  let _ =
    match Enums.sf_arg_count s with
    | Some cnt -> assert_length tail cnt lc
    | None -> ()
  in

  match s with
  | Quote -> (fst @@ List.hd tail, lc)
  | Typeof -> (Str (show_type (eval @@ List.hd tail)), lc)
  | Cons -> (
      let hd = eval @@ List.hd tail in
      let arg2 = eval @@ List.hd @@ List.tl tail in
      match arg2 with
      | List tl, _ -> (List (hd :: tl), lc)
      | _, lc2 -> raise @@ EvalError ("Expected cons list", lc2))
  | Car -> (
      match eval @@ List.hd tail with
      | List l, lc2 -> (
          match l with
          | [] -> raise @@ EvalError ("Expected nonempty list", lc2)
          | (x, _) :: _ -> (x, lc))
      | _, lc2 -> raise @@ EvalError ("Expected cons list", lc2))
  | Cdr -> (
      match eval @@ List.hd tail with
      | List l, lc2 -> (
          match l with
          | [] -> raise @@ EvalError ("Expected nonempty list", lc2)
          | (_, _) :: xs -> (List xs, lc))
      | _, lc2 -> raise @@ EvalError ("Expected cons list", lc2))
  | Cond -> (
      if List.length tail mod 2 = 0 then
        raise @@ EvalError ("Expected an odd number of arguments", lc)
      else
        match tail with
        | [] -> raise Unreachable
        | x :: [] -> eval x
        | x :: y :: xs -> (
            match eval x with
            | Bool cond, lc2 ->
                if cond then
                  eval y
                else
                  handle_sf s xs lc2
            | _, lc2 -> raise @@ EvalError ("Expected bool", lc2)))
  | Print ->
      let reprs = List.map (fun x -> x |> eval |> repr) tail in
      print_endline @@ String.concat " " reprs;
      (List [], lc)
  | Read -> (
      let reprs = List.map repr tail in
      print_endline @@ String.concat " " reprs;
      match In_channel.input_line stdin with
      | None -> (List [], lc)
      | Some line -> (
          match Parser.parse (explod line, 0) with
          | Failure (reason, _, _) ->
              raise @@ EvalError ("Input parse error:\n" ^ reason, lc)
          | Success (value, _) -> value))
  | Eval -> (tail |> List.hd |> eval |> eval |> fst, lc)
  | Symbol -> (Symbol (List.hd tail |> eval |> repr |> make_symbol), lc)
  | Do -> (
      match tail with
      | [] -> (List [], lc)
      | x :: xs ->
          let _ = eval x in
          handle_sf s xs lc)

and eval_list (l : ast list) (lc : loc) : ast =
  match l with
  | [] -> (List [], lc)
  | unevaled_head :: tail -> (
      match eval unevaled_head with
      | Op o, _ -> fold_op o tail lc
      | Pr p, _ -> fold_pr p tail lc
      | Sf s, _ -> handle_sf s tail lc
      | _ -> raise @@ EvalError ("Head not callable", lc))

and eval ((value, lc) : ast) : ast =
  match value with List l -> eval_list l lc | _ -> (value, lc)
