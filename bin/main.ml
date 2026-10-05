type symbol = string

let make_symbol str = "\x00" ^ str
let is_symbol value = String.length value > 0 && value.[0] = '\x00'
let symbol_name symbol = String.sub symbol 1 (String.length symbol - 1)

type ast =
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | List of ast list
  | Symbol of symbol

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
            (* TODO check theres a filename and the file opens goodly *)
            let file_content =
              let drop_first count str =
                String.sub str count (String.length str - count)
              in
              In_channel.with_open_text (drop_first 3 line) In_channel.input_all
            in
            Ok file_content
        | _ -> Error UnknownCommand)

let evaluate value = Str value

let rec show value =
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

let rec main_loop () =
  print_string "\n>>> ";
  flush stdout;

  let _ =
    match read () with
    | Ok res -> evaluate res |> show |> print_endline
    | Error err -> (
        match err with
        | CantRead -> exit 0
        | UnknownCommand -> print_endline "Unknown command")
  in

  main_loop ()

let () = main_loop ()

(* let () = *)
(*   print_endline @@ show *)
(*   @@ List *)
(*        [ *)
(*          List [ Int 1; Int 2; Symbol (symbol "iamasymbol") ]; *)
(*          Str "mraow"; *)
(*          Bool true; *)
(*        ] *)
