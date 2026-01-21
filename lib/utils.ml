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

(* Like [split_once], but tries multiple separators and reports which one matched.
   Returns: (matched_separator, before, after).
   If none match, returns: ('\000', s, ""). *)
let split_once_any seps s =
  let rec find_first = function
    | [] -> None
    | sep :: rest -> (
        match String.index_opt s sep with
        | None -> find_first rest
        | Some i -> Some (sep, i)
      )
  in
  match find_first seps with
  | None -> ('\000', s, "")
  | Some (sep, i) ->
      let first = String.sub s 0 i in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      (sep, first, rest)