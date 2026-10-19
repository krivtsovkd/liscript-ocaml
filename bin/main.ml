open Ast
open Err

type read_error = CantRead | UnknownCommand | FileNotFound
type read_result = ReplStr of string | FileStr of string | Error of read_error

let is_file (res : read_result) : bool =
  match res with FileStr _ -> true | _ -> false

let read_file (filename : string) : read_result =
  try FileStr (In_channel.with_open_text filename In_channel.input_all)
  with Sys_error _ -> Error FileNotFound

let read () : read_result =
  match LNoise.linenoise ">>> " with
  (* match In_channel.input_line stdin with *)
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

let rec process (orig_program : string) ((st, lc) : Parser.stream)
    (is_file : bool) (env : envt) : envt =
  match Parser.parse (st, lc) with
  | Failure (msg, loc_start, loc_end) ->
      print_error orig_program msg (loc_start, loc_end);
      env
  | Success (value, rest_stream) -> (
      let env, out =
        try
          let res, env = Eval.eval env value in
          let res_str = show_ast res in
          cond_print_endline (not is_file) res_str;
          (env, res_str)
        with EvalError (msg, lc) ->
          print_error orig_program msg lc;
          (env, "")
      in

      match rest_stream with
      | [], _ | [ '\n' ], _ ->
          cond_print_endline (is_file && out <> "") out;
          env
      | st -> process orig_program st is_file env)

let rec main_loop (env : envt) : envt =
  let env =
    let read_res = read () in
    match read_res with
    | Error err ->
        (match err with
        | CantRead -> exit 0
        | UnknownCommand -> print_endline "Unknown command"
        | FileNotFound -> print_endline "File not found");
        env
    | ReplStr res | FileStr res ->
        LNoise.history_add res |> ignore;
        let _ =
          match
            LNoise.history_save ~filename:"/home/yuli/temp/lisphist.txt"
          with
          | Ok _ -> ()
          | Error msg -> print_endline msg
        in
        process res (explod res, 0) (is_file read_res) env
  in

  main_loop env

let () =
  let stdlib_str =
    match read_file "stdlib/lib.ls" with
    | FileStr s | ReplStr s -> s
    | Error _ ->
        print_endline "Stdlib file not found";
        exit 0
  in

  let env = process stdlib_str (explod stdlib_str, 0) true StringMap.empty in
  LNoise.history_load ~filename:"/home/yuli/temp/lisphist.txt" |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;

  main_loop env |> ignore
