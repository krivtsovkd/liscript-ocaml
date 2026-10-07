open Ast

type read_error = CantRead | UnknownCommand

let read () =
  match In_channel.input_line stdin with
  | None -> Error CantRead
  | Some line -> (
      if line.[0] <> ':' then
        Ok line
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
            Ok file_content
        | _ -> Error UnknownCommand)

let evaluate value = Str value
let explod str = List.init (String.length str) (String.get str)

let rec main_loop () =
  print_string "\n>>> ";
  flush stdout;

  let _ =
    match read () with
    | Error err -> (
        match err with
        | CantRead -> exit 0
        | UnknownCommand -> print_endline "Unknown command")
    | Ok res -> (
        match explod res |> Parser.ast_parser () with
        | Failure reason -> print_endline reason
        | Success (value, _) -> show value |> print_endline)
  in

  main_loop ()

let () = main_loop ()

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
