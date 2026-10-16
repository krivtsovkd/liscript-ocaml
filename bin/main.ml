open Ast

let check_a (env : Eval.Env.t) =
  Out_channel.output_string stderr
  @@ string_of_bool
  @@ Eval.Env.StringMap.exists (fun s _ -> "a" = Ast.symbol_name s) env;
  flush stderr

type read_error = CantRead | UnknownCommand
type read_result = ReplStr of string | FileStr of string | Error of read_error

let read () : read_result =
  match In_channel.input_line stdin with
  | None -> Error CantRead
  | Some line -> (
      if line.[0] <> ':' then
        ReplStr line
      else
        match line.[1] with
        | 'l' ->
            let file_content =
              let drop_first count str =
                String.sub str count (String.length str - count)
              in
              try
                In_channel.with_open_text (drop_first 3 line)
                  In_channel.input_all
              with Sys_error _ -> "File_not_found " (* TODO *)
            in
            FileStr file_content
        | _ -> Error UnknownCommand)

let explod str = List.init (String.length str) (String.get str)

let rec pre_nth_line_start (str : string) (n : int) : int =
  if n = 0 then
    -1
  else
    let lines = String.split_on_char '\n' str in
    let line_sizes = List.map (fun s -> 1 + String.length s) lines in
    let len_sum = List.take n line_sizes |> List.fold_left ( + ) 0 in
    len_sum - 1

let get_line (str : string) (linenum : int) : string =
  let line_start = 1 + pre_nth_line_start str linenum in
  let line_end = pre_nth_line_start str (linenum + 1) in
  String.sub str line_start (line_end - line_start)

let offset_to_linenum (str : string) (offset : int) : int =
  let prefix = String.sub str 0 offset in
  List.length @@ List.filter (fun c -> c = '\n') @@ explod prefix

let print_error (program : string) (msg : string) ((loc_start, loc_end) : loc) =
  let loc_end = loc_end - 1 in

  let start_linenum = offset_to_linenum program loc_start in
  let start_line = get_line program start_linenum in
  let start_line_offset = pre_nth_line_start program start_linenum in
  let start_column = loc_start - start_line_offset - 1 in
  let start_str = String.init start_column (fun _ -> ' ') in

  print_endline @@ "\x1b[31mError\x1b[39m on line "
  ^ string_of_int (start_linenum + 1)
  ^ ":";
  print_endline msg;
  print_endline start_line;

  print_string "\x1b[31m";

  let end_linenum = offset_to_linenum program loc_end in
  let end_line_offset = pre_nth_line_start program end_linenum in
  let end_column = loc_end - end_line_offset - 1 in
  if end_linenum = start_linenum then (
    if start_column = end_column then
      let _ = print_endline @@ start_str ^ "^" in
      print_string "\x1b[39m"
    else
      let tildes = String.init (end_column - start_column - 1) (fun _ -> '~') in
      print_endline @@ start_str ^ "^" ^ tildes ^ "^";
      print_string "\x1b[39m"
  ) else
    let end_line = get_line program end_linenum in
    let line_rest = String.length start_line - start_column - 1 in
    let tildes1 = String.init line_rest (fun _ -> '~') in
    let tildes2 = String.init end_column (fun _ -> '~') in

    print_endline @@ start_str ^ "^" ^ tildes1 ^ "\n";
    print_string "\x1b[39m";
    print_endline end_line;
    print_string "\x1b[31m";
    print_endline @@ tildes2 ^ "^";
    print_string "\x1b[39m"

let cond_print_endline (cond : bool) (str : string) =
  if cond then
    print_endline str
  else
    ()

let rec process (orig_program : string) ((st, lc) : Parser.stream)
    (is_file : bool) (env : Eval.Env.t) : Eval.Env.t =
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
        with Eval.EvalError (msg, lc) ->
          print_error orig_program msg lc;
          (env, "")
      in

      match rest_stream with
      | [], _ | [ '\n' ], _ ->
          cond_print_endline (is_file && out <> "") out;
          env
      | st -> process orig_program st is_file env)

let rec main_loop (env : Eval.Env.t) : Eval.Env.t =
  print_string "\n>>> ";
  flush stdout;

  let env =
    match read () with
    | Error err ->
        (match err with
        | CantRead -> exit 0
        | UnknownCommand -> print_endline "Unknown command");
        env
    | ReplStr res -> process res (explod res, 0) false env
    | FileStr res -> process res (explod res, 0) true env
  in

  main_loop env

let () =
  let _ = main_loop Eval.Env.StringMap.empty in
  ()

(* let () = *)
(*   print_endline @@ show *)
(*   @@ List *)
(*        [ *)
(*          List [ Int 1; Int 2; Symbol (make_symbol "iamasymbol") ]; *)
(*          Str "mraow"; *)
(*          Bool true; *)
(*        ] *)

(* let () = *)
(*   match (Parser.ast_parser ()) (explod "(\"string\" ;fuck \n (()(123)))") with *)
(*   | Failure reason -> print_endline reason *)
(*   | Success (value, _) -> print_endline (Ast.show value) *)
