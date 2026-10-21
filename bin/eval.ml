open Ast
open Util

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

let op_funs (o : Enums.op) (a : ast) (b : ast) (lc : loc) (trace : tracet) : ast
    =
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
                 lc,
                 trace ))

let pr_num (p : Enums.pr) (x : 'a) (y : 'a) : bool =
  match p with
  | Eq -> x = y
  | Gt -> x > y
  | Lt -> x < y
  | Gte -> x >= y
  | Lte -> x <= y

let pr_funs (p : Enums.pr) (a : ast) (b : ast) (lc : loc) (trace : tracet) :
    bool =
  match p with
  | Eq -> (
      match (a, b) with
      | (List al, _), (List bl, _) -> lists_eq al bl
      | _, _ -> fst a = fst b)
  | Gt | Lt | Gte | Lte -> (
      match (a, b) with
      | (Int x, _), (Int y, _) -> pr_num p x y
      | (Float x, _), (Float y, _) -> pr_num p x y
      | _ ->
          raise
          @@ EvalError
               ( "Cant apply predicate to " ^ show_ast a ^ " and " ^ show_ast b,
                 lc,
                 trace ))

let assert_length (args : ast list) (len : int) (lc : loc) (trace : tracet) =
  if List.length args <> len then
    raise
    @@ EvalError
         ( "Expected " ^ string_of_int len ^ " arguments, got " ^ string_of_int
           @@ List.length args,
           lc,
           trace )

let rec fold_op (op : Enums.op) (values : ast list) (lc : loc) (env : envt)
    (trace : tracet) : ast * envt =
  match values with
  | [] -> raise @@ EvalError ("Operation on empty list", lc, trace)
  | x :: xs ->
      let x =
        if op = Enums.Concat then
          (Str (repr x), snd x)
        else
          x
      in
      let rec loop (acc : ast) (env : envt) (rest : ast list) : ast * envt =
        match rest with
        | [] -> (acc, env)
        | x :: xs ->
            let x, env = eval env trace x in
            loop (op_funs op acc x lc trace) env xs
      in
      let x, env = eval env trace x in
      loop x env xs

and fold_pr (p : Enums.pr) (values : ast list) (lc : loc) (env : envt)
    (trace : tracet) : ast * envt =
  let rec loop (vals : ast list) (env : envt) : ast_data * envt =
    match vals with
    | [] | [ _ ] -> (Bool true, env)
    | x :: y :: xs ->
        let ey, env = eval env trace y in
        if not (pr_funs p x ey lc trace) then
          (Bool false, env)
        else
          loop (ey :: xs) env
  in
  match values with
  | [] -> ((Bool true, lc), env)
  | x :: xs ->
      let x, env = eval env trace x in
      let res, env = loop (x :: xs) env in
      ((res, lc), env)

and handle_sf (s : Enums.sf) (tail : ast list) (lc : loc) (env : envt)
    (trace : tracet) : ast * envt =
  let _ =
    match Enums.sf_arg_count s with
    | Some cnt -> assert_length tail cnt lc trace
    | None -> ()
  in

  match s with
  | Quote -> ((fst @@ List.hd tail, lc), env)
  | Typeof ->
      let s, env = eval env trace @@ List.hd tail in
      ((Str (show_type s), lc), env)
  | Cons -> (
      let hd, env = eval env trace @@ List.hd tail in
      let arg2, env = eval env trace @@ List.hd @@ List.tl tail in
      match arg2 with
      | List tl, _ -> ((List (hd :: tl), lc), env)
      | _, lc2 ->
          raise
          @@ EvalError ("Expected cons list, got " ^ show_ast arg2, lc2, trace))
  | Car -> (
      let arg, env = eval env trace @@ List.hd tail in
      match arg with
      | List l, lc2 -> (
          match l with
          | [] ->
              raise
              @@ EvalError
                   ( "Expected nonempty list, got " ^ show_data (List l),
                     lc2,
                     trace )
          | (x, _) :: _ -> ((x, lc), env))
      | res, lc2 ->
          if Settings.implicit_list_interface then
            (arg, env)
          else
            raise
            @@ EvalError ("Expected cons list, got " ^ show_data res, lc2, trace)
      )
  | Cdr -> (
      let arg, env = eval env trace @@ List.hd tail in
      match arg with
      | List l, lc2 -> (
          match l with
          | [] ->
              raise
              @@ EvalError
                   ( "Expected nonempty list, got " ^ show_data (List l),
                     lc2,
                     trace )
          | (_, _) :: xs -> ((List xs, lc), env))
      | res, lc2 ->
          if Settings.implicit_list_interface then
            ((List [], lc), env)
          else
            raise
            @@ EvalError ("Expected cons list, got " ^ show_data res, lc2, trace)
      )
  | Cond -> (
      if List.length tail mod 2 = 0 then
        raise
        @@ EvalError
             ( "Expected an odd number of arguments, got "
               ^ string_of_int (List.length tail),
               lc,
               trace )
      else
        match tail with
        | [] -> raise Unreachable
        | x :: [] -> eval env trace x
        | x :: y :: xs -> (
            let x, env = eval env trace x in
            match x with
            | Bool cond, lc2 ->
                if cond then
                  eval env trace y
                else
                  handle_sf s xs lc2 env trace
            | res, lc2 ->
                raise
                @@ EvalError ("Expected bool, got " ^ show_data res, lc2, trace)
            ))
  | Print ->
      let rec loop (acc : string list) (rest : ast list) (env : envt) :
          string list * envt =
        match rest with
        | [] -> (acc, env)
        | x :: xs ->
            let x, env = eval env trace x in
            loop (repr x :: acc) xs env
      in
      let reprs, env = loop [] tail env in
      let reprs = List.rev reprs in
      print_endline @@ String.concat " " reprs;
      ((List [], lc), env)
  | Read -> (
      (* TODO not copypaste print *)
      let rec loop (acc : string list) (rest : ast list) (env : envt) :
          string list * envt =
        match rest with
        | [] -> (acc, env)
        | x :: xs ->
            let x, env = eval env trace x in
            loop (repr x :: acc) xs env
      in
      let reprs, env = loop [] tail env in
      let reprs = List.rev reprs in
      print_endline @@ String.concat " " reprs;

      match In_channel.input_line stdin with
      | None -> ((List [], lc), env)
      | Some line -> (
          match Parser.parse (explod line, 0, "") with
          | Failure (reason, _) ->
              raise @@ EvalError ("Input parse error:\n" ^ reason, lc, trace)
          | Success (value, _) -> (value, env)))
  | Symbol ->
      let s, env = eval env trace @@ List.hd tail in
      ((Symbol (s |> repr |> Symbol.make), lc), env)
  | Eval ->
      let x, env = eval env trace @@ List.hd tail in
      let x', env = eval env trace x in
      ((fst x', lc), env)
  | Do -> (
      match tail with
      | [] -> ((List [], lc), env)
      | x :: [] -> eval env trace x
      | x :: xs ->
          let _, env = eval env trace x in
          handle_sf s xs lc env trace)
  | Def | Set -> (
      let f =
        if s = Def then
          env_def
        else
          env_set
      in
      match tail with
      | [] -> ((List [], lc), env)
      | _ :: [] ->
          raise
          @@ EvalError
               ( "Expected an even number of values, got "
                 ^ string_of_int (List.length tail),
                 lc,
                 trace )
      | sym :: value :: xs -> (
          match sym with
          | Symbol sym, _ ->
              let (evalue, _), env = eval env trace value in
              let env = f env sym evalue in
              handle_sf s xs lc env trace
          | _, lc' ->
              raise
              @@ EvalError ("Expected symbol, got " ^ show_ast sym, lc', trace))
      )
  | Lambda | Dambda -> (
      let make_f =
        if s = Lambda then
          make_lambda
        else
          make_dambda
      in
      let args, body = (List.hd tail, List.nth tail 1) in
      match args with
      | List args, _ -> ((Lambda (make_f args body env), lc), env)
      | _, loc_args ->
          raise
          @@ EvalError
               ("Expected cons list, got " ^ show_ast args, loc_args, trace))
  | Macro -> (
      let args, body = (List.hd tail, List.nth tail 1) in
      match args with
      | List args, _ -> ((Macro { args; body }, lc), env)
      | _, loc_args ->
          raise
          @@ EvalError
               ("Expected cons list, got " ^ show_ast args, loc_args, trace))
  | Panic ->
      let error_msg, env = eval env trace @@ List.hd tail in
      raise @@ EvalError ("Panic: " ^ show_ast error_msg, lc, trace)
  | Python -> ((Bool false, lc), env)
  | CurTime -> ((Float (Unix.gettimeofday ()), lc), env)

and eval_lambda (l : lambda) (passed_values : ast list) (lc : loc) (env_ : envt)
    (trace : tracet) : ast * envt =
  let make_f =
    if is_dambda l then
      make_dambda
    else
      make_lambda
  in

  let rec loop (lambda_args : ast list) (passed_values : ast list)
      (lam_env : envt) (outside_env : envt) : ast * envt =
    match (lambda_args, passed_values) with
    | [], passed_values ->
        let outside_env, evaled_values =
          List.fold_left_map
            (fun acc_env val_elem ->
              eval acc_env trace val_elem |> fun (a, b) -> (b, a))
            outside_env passed_values
        in
        let lam_env =
          env_def lam_env (Symbol.make "rest-args") (List evaled_values)
        in
        (fst @@ eval lam_env trace l.body, outside_env)
    | lambda_args, [] ->
        if is_dambda l then
          raise @@ EvalError ("Partial dambda not allowed", lc, trace)
        else
          ((Lambda (make_f lambda_args l.body lam_env), lc), outside_env)
    | (Symbol sym, _) :: args, value :: values ->
        let value, outside_env = eval outside_env trace value in
        let lam_env = env_def lam_env sym (fst value) in
        loop args values lam_env outside_env
    | (res, arg_loc) :: _, _ ->
        raise
        @@ EvalError ("Expected symbol, got " ^ show_data res, arg_loc, trace)
  in

  loop l.args passed_values (l.get_env env_) env_

and macroexpand (env : envt) (trace : tracet) ((value, lc) : ast) : ast =
  match value with
  | Symbol s -> (
      try (env_get env trace (value, lc), lc) with EvalError _ -> (value, lc))
  | List l -> (
      match l with
      | [] -> (List [], lc)
      | x :: xs -> (
          let mx = macroexpand env trace x in
          match macroexpand env trace (List xs, lc) with
          | List mxs, _ -> (List (mx :: mxs), lc)
          | _ -> raise Unreachable))
  | _ -> (value, lc)

and eval_macro (m : macro) (passed_values : ast list) (lc : loc)
    (outside_env : envt) (trace : tracet) : ast * envt =
  let rec loop (macro_args : ast list) (passed_values : ast list) (env : envt) :
      ast * envt =
    match (macro_args, passed_values) with
    | [], passed_values ->
        let env = env_def env (Symbol.make "rest-args") (List passed_values) in
        eval outside_env trace @@ macroexpand env trace m.body
    | macro_args, [] ->
        ( (Macro { args = macro_args; body = macroexpand env trace m.body }, lc),
          outside_env )
    | (Symbol sym, _) :: args, value :: values ->
        let env = env_def env sym (fst value) in
        loop args values env
    | (res, arg_loc) :: _, _ ->
        raise
        @@ EvalError ("Expected symbol, got " ^ show_data res, arg_loc, trace)
  in
  loop m.args passed_values StringMap.empty

and eval_list (l : ast list) (lc : loc) (env : envt) (trace : tracet) :
    ast * envt =
  match l with
  | [] -> ((List [], lc), env)
  | unevaled_head :: tail -> (
      match eval env trace unevaled_head with
      | (Op o, _), env -> fold_op o tail lc env trace
      | (Pr p, _), env -> fold_pr p tail lc env trace
      | (Sf s, _), env -> handle_sf s tail lc env trace
      | (Lambda l, _), env -> eval_lambda l tail lc env trace
      | (Macro m, _), env -> eval_macro m tail lc env trace
      | head, _ ->
          raise
          @@ EvalError ("Head not callable: " ^ show_data (List l), lc, trace))

and eval (env : envt) (trace : tracet) ((value, lc) : ast) : ast * envt =
  match value with
  | Symbol _ -> ((env_get env trace (value, lc), lc), env)
  | List l -> eval_list l lc env (lc :: trace)
  | _ -> ((value, lc), env)
