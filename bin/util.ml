exception Unreachable

let explod str = List.init (String.length str) (String.get str)

let count_chars (st : char list) (ch : char) : int =
  List.length @@ List.filter (fun c -> c = ch) st

let count_chars_str (s : string) (ch : char) : int = count_chars (explod s) ch
