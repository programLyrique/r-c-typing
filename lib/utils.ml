let is_singleton l = 
  match l with
  | [_] -> true
  | _ -> false

let split_once sep s =
  try
    let i = String.index s sep in
    let first = String.sub s 0 i in
    let rest = String.sub s (i + 1) (String.length s - i - 1) in
    (first, rest)
  with Not_found -> (s, "")