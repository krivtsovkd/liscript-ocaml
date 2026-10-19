type op = Add | Sub | Mul | Div | Mod | Concat

let op_str_list =
  [ (Concat, "++"); (Add, "+"); (Sub, "-"); (Mul, "*"); (Div, "/"); (Mod, "%") ]

type pr = Eq | Gt | Lt | Gte | Lte

let pr_str_list = [ (Eq, "="); (Gte, ">="); (Lte, "<="); (Gt, ">"); (Lt, "<") ]

type sf =
  | Quote
  | Typeof
  | Cons
  | Car
  | Cdr
  | Cond
  | Print
  | Read
  | Eval
  | Symbol
  | Do
  | Def
  | Set
  | Lambda
  | Dambda

let sf_str_list =
  [
    (Quote, "quote");
    (Typeof, "typeof");
    (Cons, "cons");
    (Car, "car");
    (Cdr, "cdr");
    (Cond, "cond");
    (Print, "print");
    (Eval, "eval");
    (Read, "read");
    (Symbol, "symbol");
    (Do, "do");
    (Def, "def");
    (Set, "set!");
    (Lambda, "lambda");
    (Dambda, "dambda");
  ]

let sf_arg_count (s : sf) : int option =
  match s with
  | Quote -> Some 1
  | Typeof -> Some 1
  | Cons -> Some 2
  | Car -> Some 1
  | Cdr -> Some 1
  | Eval -> Some 1
  | Symbol -> Some 1
  | Lambda -> Some 2
  | Dambda -> Some 2
  | _ -> None

exception Unreachable

let string_of_val (val_list : ('a * string) list) (v : 'a) : string =
  let rec loop rest_v =
    match rest_v with
    | [] -> raise Unreachable
    | (x, str) :: xs ->
        if x = v then
          str
        else
          loop xs
  in
  loop val_list

let val_of_string (val_list : ('a * string) list) (s : string) : 'a option =
  let rec loop rest_v =
    match rest_v with
    | [] -> None
    | (x, str) :: xs ->
        if s = str then
          Some x
        else
          loop xs
  in
  loop val_list

let string_of_op : op -> string = string_of_val op_str_list
let op_of_string : string -> op option = val_of_string op_str_list
let string_of_pr : pr -> string = string_of_val pr_str_list
let pr_of_string : string -> pr option = val_of_string pr_str_list
let string_of_sf : sf -> string = string_of_val sf_str_list
let sf_of_string : string -> sf option = val_of_string sf_str_list
