open Ast
open Err
open Util

type read_error = CantRead | UnknownCommand | FileNotFound

type read_result =
  | ReplStr of string
  | FileStr of string * string
  | Error of read_error

let is_file (res : read_result) : bool =
  match res with FileStr _ -> true | _ -> false

let read_file (filename : string) : read_result =
  try FileStr (In_channel.with_open_text filename In_channel.input_all, filename)
  with Sys_error _ -> Error FileNotFound

let read () : read_result =
  match LNoise.linenoise ">>> " with
  | None -> Error CantRead
  | Some line -> (
      if line.[0] <> ':' then
        ReplStr line
      else
        match line.[1] with
        | 'l' -> read_file @@ String.sub line 3 (String.length line - 3)
        | _ -> Error UnknownCommand)

let cond_print_endline (cond : bool) (str : string) =
  if cond then
    print_endline str
  else
    ()

exception Unreachable

let rec process ((st, lc, fname) : Parser.stream) (is_file : bool) (env : envt)
    (hist : string StringMap.t) : envt * string StringMap.t =
  match Parser.parse (st, lc, fname) with
  | Failure (msg, lc) ->
      print_error hist msg lc [];
      (env, hist)
  | Success (value, rest_stream) -> (
      let env, out =
        try
          let res, env = Eval.eval env [] value in
          let res_str = show_ast res in
          cond_print_endline (not is_file) res_str;
          (env, res_str)
        with EvalError (msg, lc, trace) ->
          print_error hist msg lc trace;
          (env, "")
      in

      match Parser.is_whitespace_only () rest_stream with
      | Failure _ -> raise Unreachable
      | Success (true, _) ->
          cond_print_endline (is_file && out <> "") out;
          (env, hist)
      | Success (false, st) -> process st is_file env hist)

let rec main_loop (env : envt) (hist : string StringMap.t) : envt =
  let env, hist =
    let read_res = read () in
    match read_res with
    | Error err ->
        (match err with
        | CantRead -> exit 0
        | UnknownCommand -> print_endline "Unknown command"
        | FileNotFound -> print_endline "File not found");
        (env, hist)
    | ReplStr res ->
        LNoise.history_add res |> ignore;
        let fname = "repl" ^ string_of_int @@ Random.int 1000000000 in
        let hist = StringMap.add fname res hist in
        process (explod res, 0, fname) false env hist
    | FileStr (res, fname) ->
        LNoise.history_add (":l " ^ fname) |> ignore;

        let hist = StringMap.add fname res hist in
        process (explod res, 0, fname) true env hist
  in

  main_loop env hist

let setup_lnoise () =
  LNoise.history_set ~max_length:1000 |> ignore;
  LNoise.set_completion_callback (fun line completions ->
      let bopen = count_chars_str line '(' in
      let bose = count_chars_str line ')' in
      if bose >= bopen then
        ()
      else
        LNoise.add_completion completions
          (line ^ String.init (bopen - bose) (fun _ -> ')')))

let () =
  let stdlib_str, stdlib_fname =
    match read_file "stdlib/lib.ls" with
    | FileStr (s, fname) -> (s, fname)
    | ReplStr _ ->
        print_endline "Unreachable";
        exit 0
    | Error _ ->
        print_endline "Stdlib file not found";
        exit 0
  in

  let hist = StringMap.add stdlib_fname stdlib_str StringMap.empty in

  let env, hist =
    process (explod stdlib_str, 0, stdlib_fname) true StringMap.empty hist
  in

  setup_lnoise ();

  main_loop env hist |> ignore
