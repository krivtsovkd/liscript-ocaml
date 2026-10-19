open Ast

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

let print_error_impl (program : string) (msg : string)
    ((loc_start, loc_end) : loc) =
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

let print_error (program : string) (msg : string) ((loc_start, loc_end) : loc) =
  print_error_impl program msg (loc_start, loc_end);
  flush stdout
