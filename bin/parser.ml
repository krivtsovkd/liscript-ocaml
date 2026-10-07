open Ast

type stream = char list
type 'a parse_result = Success of ('a * stream) | Failure of string
type 'a parser = stream -> 'a parse_result

let predicate_parser (pred : char -> bool) : char parser =
 fun st ->
  match st with
  | [] -> Failure "Empty stream"
  | x :: xs ->
      if pred x then
        Success (x, xs)
      else
        Failure
          ("Predicate false at " ^ String.make 1 x
          ^ String.concat ""
              (List.take 30 xs |> List.map (fun c -> String.make 1 c)))

let anychar_parser = predicate_parser (fun _ -> true)
let digits = "0123456789"
let whitespaces = " \n\r\t"
let delimiters = whitespaces ^ "()\";,"
let first_in_symbol = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let charset_parser (charset : string) =
  predicate_parser (fun c -> String.contains charset c)

let char_parser (c : char) = predicate_parser (fun c' -> c' == c)
let return (x : 'a) : 'a parser = fun st -> Success (x, st)
let error (msg : string) : 'a parser = fun st -> Failure msg

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

let rec str_parser (str : string) : string parser =
  if String.length str = 0 then
    return ""
  else
    let c = str.[0] in
    let* _ = char_parser c in
    let* _ = str_parser (String.sub str 1 (String.length str - 1)) in
    return str

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
 fun st ->
  match p1 st with
  | Success (x, st') -> Success (x, st')
  | Failure reason -> (
      match p2 st with
      | Success (x, st') -> Success (x, st')
      | Failure reason2 -> Failure reason2)

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
  let* _ = char_parser '"' in

  let rec loop (acc : string) : string parser =
    alt
      (let* _ = char_parser '"' in
       return acc)
      (let* c = anychar_parser in
       loop (acc ^ String.make 1 c))
  in
  loop ""

let symbol_parser () : symbol parser =
  let* first = charset_parser first_in_symbol in

  let rec loop (acc : string) : string parser =
    alt
      (let* _ = peek @@ charset_parser delimiters in
       return acc)
      (let* c = anychar_parser in
       loop (acc ^ String.make 1 c))
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

and ast_parser () : ast parser =
  let* _ = whitespace_parser () in
  alt
    (let* f = float_parser () in
     return @@ Float f)
  @@ alt
       (let* i = int_parser () in
        return @@ Int i)
  @@ alt
       (let* b = bool_parser () in
        return @@ Bool b)
  @@ alt
       (let* s = string_parser () in
        return @@ Str s)
  @@ alt
       (let* l = list_parser () in
        return @@ List l)
       (let* s = symbol_parser () in
        return @@ Symbol s)
