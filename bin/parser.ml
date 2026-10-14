open Ast

type stream = char list * int

type 'a parse_result =
  | Success of ('a * stream)
  | Failure of (string * int * int)

type 'a parser = stream -> 'a parse_result

let predicate_parser (pred : char -> bool) : char parser =
 fun (st, loc) ->
  match st with
  | [] -> Failure ("Empty stream", loc, loc + 1)
  | x :: xs ->
      if pred x then
        Success (x, (xs, loc + 1))
      else
        Failure ("Predicate false", loc, loc + 1)

let anychar_parser = predicate_parser (fun _ -> true)
let digits = "0123456789"
let whitespaces = " \n\r\t"
let delimiters = whitespaces ^ "()\";,"
let first_in_symbol = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let charset_parser (charset : string) =
  predicate_parser (fun c -> String.contains charset c)

let char_parser (c : char) = predicate_parser (fun c' -> c' == c)
let return (x : 'a) : 'a parser = fun st -> Success (x, st)

let error (msg : string) (loc_start : int) (loc_end : int) : 'a parser =
 fun st -> Failure (msg, loc_start, loc_end)

let get_loc () : int parser = fun (st, loc) -> Success (loc, (st, loc))

let bind (m : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun st ->
  match m st with
  | Failure reason -> Failure reason
  | Success (x, st') -> f x st'

let ( let* ) = bind

let peek (m : 'a parser) : 'a parser =
 fun st ->
  match m st with
  | Failure reason -> Failure reason
  | Success (x, _) -> Success (x, st)

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
 fun st ->
  match p1 st with
  | Success (x, st') -> Success (x, st')
  | Failure reason -> (
      match p2 st with
      | Success (x, st') -> Success (x, st')
      | Failure reason2 -> Failure reason2)

let rec str_parser (str : string) : string parser =
  if String.length str = 0 then
    return ""
  else
    let c = str.[0] in
    let* _ = char_parser c in
    let* _ = str_parser (String.sub str 1 (String.length str - 1)) in
    return str

let comment_parser () : unit parser =
  let* _ = char_parser ';' in

  let rec loop () =
    alt
      (let* _ = char_parser '\n' in
       return ())
      (let* _ = anychar_parser in
       loop ())
  in
  loop ()

let whitespace_parser () : unit parser =
  let rec loop () =
    alt
      (let* _ = charset_parser whitespaces in
       loop ())
    @@ alt
         (let* _ = comment_parser () in
          loop ())
         (return ())
  in
  loop ()

let int_parser () : int parser =
  let int_of_char c = Char.code c - Char.code '0' in
  let* _ = peek @@ charset_parser digits in

  let rec loop (acc : int) : int parser =
    alt
      (let* d = charset_parser digits in
       loop ((acc * 10) + int_of_char d))
      (return acc)
  in
  loop 0

let bool_parser () : bool parser =
  alt
    (let* _ = str_parser "true" in
     return true)
    (let* _ = str_parser "false" in
     return false)

let string_parser () : string parser =
  let* initial_loc = get_loc () in
  let* _ = char_parser '"' in

  let rec loop (acc : string) : string parser =
    alt
      (let* _ = char_parser '"' in
       return acc)
    @@ alt
         (let* c = anychar_parser in
          loop (acc ^ String.make 1 c))
         (error "Missing closing bracket" initial_loc (initial_loc + 1))
  in
  loop ""

let symbol_parser () : symbol parser =
  let* first = charset_parser first_in_symbol in

  let rec loop (acc : string) : string parser =
    alt
      (let* _ = peek @@ charset_parser delimiters in
       return acc)
    @@ alt
         (let* c = anychar_parser in
          loop (acc ^ String.make 1 c))
         (return acc)
  in
  loop @@ "\x00" ^ String.make 1 first

let float_parser () : float parser =
  let rec loop1 (acc : string) : string parser =
    alt
      (let* _ = char_parser '.' in
       return acc)
      (let* d = charset_parser digits in
       loop1 (acc ^ String.make 1 d))
  in
  let rec loop2 (acc : string) : string parser =
    alt
      (let* d = charset_parser digits in
       loop2 (acc ^ String.make 1 d))
      (return acc)
  in

  let* first = loop1 "" in
  let* second = loop2 "" in
  return @@ Float.of_string (first ^ "." ^ second)

let val_parser (val_str_list : ('a * string) list) : 'a parser =
  let parser_list =
    val_str_list
    |> List.map (fun (x, str) ->
        let* _ = str_parser str in
        return x)
  in
  List.fold_right alt parser_list
    (error "Expected keyword, didnt get one 💥" 0 0)

let rec list_parser () : ast list parser =
  let* _ = char_parser '(' in
  let rec loop (acc : ast list) : ast list parser =
    let* _ = whitespace_parser () in
    alt
      (let* _ = char_parser ')' in
       return acc)
      (let* c = ast_parser () in
       loop (c :: acc))
  in
  let* l = loop [] in
  return @@ List.rev l

and quote_parser () : ast parser =
  let* init_loc = get_loc () in
  let* _ = char_parser '\'' in
  let* term = ast_parser () in
  let* final_loc = get_loc () in
  return
    ( List [ (Sf Enums.Quote, (init_loc, init_loc + 1)); term ],
      (init_loc, final_loc) )

and ast_parser () : ast parser =
  let* _ = whitespace_parser () in
  let* init_loc = get_loc () in

  let ast_return (x : ast_data) : ast parser =
    let* final_loc = get_loc () in
    return (x, (init_loc, final_loc))
  in

  alt
    (let* f = float_parser () in
     ast_return (Float f))
  @@ alt
       (let* i = int_parser () in
        ast_return (Int i))
  @@ alt
       (let* op = val_parser Enums.op_str_list in
        ast_return (Op op))
  @@ alt
       (let* pr = val_parser Enums.pr_str_list in
        ast_return (Pr pr))
  @@ alt
       (let* sf = val_parser Enums.sf_str_list in
        ast_return (Sf sf))
  @@ alt
       (let* b = bool_parser () in
        ast_return (Bool b))
  @@ alt
       (let* s = string_parser () in
        ast_return (Str s))
  @@ alt
       (let* l = list_parser () in
        ast_return (List l))
  @@ alt (quote_parser ())
  @@ alt
       (let* s = symbol_parser () in
        ast_return (Symbol s))
       (error "What is this" init_loc (init_loc + 1))

let count_chars (st : char list) (ch : char) : int =
  List.length @@ List.filter (fun c -> c = ch) st

let check_paren_balance (st : char list) : bool = true

let parse ((st, lc) : stream) : ast parse_result =
  if count_chars st '"' mod 2 <> 0 then
    Failure ("Unmatched quote", lc, lc + 1)
  else if
    count_chars st '(' != count_chars st ')' || (not @@ check_paren_balance st)
  then
    Failure ("Unmatched bracket", lc, lc + 1)
  else
    ast_parser () (st, lc)
