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

(** Query the system C compiler for its default angle-bracket include search
    paths. This covers multiarch dirs (e.g. /usr/include/x86_64-linux-gnu on
    Debian) that a hardcoded list would miss. Runs [gcc -xc -E -v -] and
    parses the block between "#include <...> search starts here:" and
    "End of search list." — the directories the compiler would search for
    [#include <foo>]. Falls back to [cc] if [gcc] is absent, and returns []
    if neither is available or parsing fails. Callers should combine the
    result with their own fallback list. *)
let detect_gcc_include_dirs () =
  let try_command cmd =
    try
      let ic = Unix.open_process_in cmd in
      let dirs = ref [] in
      let in_block = ref false in
      let done_ = ref false in
      (try
         while not !done_ do
           let line = input_line ic in
           if !in_block then begin
             if String.starts_with ~prefix:"End of search list" line then begin
               in_block := false;
               done_ := true
             end else if String.length line > 0 && line.[0] = ' ' then
               dirs := String.trim line :: !dirs
           end else if String.starts_with ~prefix:"#include <...> search starts here" line then
             in_block := true
         done
       with End_of_file -> ());
      (* Drain any remaining output so the child exits cleanly. *)
      (try while true do ignore (input_line ic) done with End_of_file -> ());
      (match Unix.close_process_in ic with
       | Unix.WEXITED 0 -> List.rev !dirs
       | _ -> [])
    with _ -> []
  in
  (* gcc is the standard; fall back to cc if gcc is absent. *)
  match try_command "gcc -xc -E -v - < /dev/null 2>&1" with
  | [] -> try_command "cc -xc -E -v - < /dev/null 2>&1"
  | dirs -> dirs