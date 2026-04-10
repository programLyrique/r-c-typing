(** Analyze a R package to get the list of entry points and their calling conventions.
We look through the R source code to find .C, .Call, .Fortran, and .External

.Call(PREFIX_funa_name, arg1, arg2, arg3)

Just with regexes, which is quite fragile.
We detect the native symbol prefix from the package root NAMESPACE file.
When `.fixes = "PREFIX_"` is present, the R variables exposed for registered
native routines are prefixed, while the underlying native symbol names remain
unprefixed.

useDynLib(libName, .registration = TRUE, .fixes = "PREFIX_")

TODO: use tree_sitter with the R grammar to extract the calls to these functions in a more robust way.
*)

(* Type to represent calling conventions *)
type calling_convention = C | Call | Fortran | External

let calling_convention_to_string = function
  | C -> "C"
  | Call -> "Call"
  | Fortran -> "Fortran"
  | External -> "External"

let find_native_calls path =
  (* Extract prefix from the package root NAMESPACE file. *)
  let find_prefix () =
    let namespace_file = Filename.concat path "NAMESPACE" in
    if not (Sys.file_exists namespace_file) then ""
    else
      try
        let lines = In_channel.with_open_text namespace_file In_channel.input_lines in
        let content = String.concat "\n" lines in
        let pattern = Str.regexp "useDynLib.*\\.fixes[ \t]*=[ \t]*\"\\([^\"]+\\)\"" in
        try
          ignore (Str.search_forward pattern content 0);
          Str.matched_group 1 content
        with Not_found -> ""
      with _ -> ""
  in

  (* Convert prefixed R variables back to the underlying native symbol name. *)
  let strip_prefix prefix func_name =
    if prefix = "" then func_name
    else if String.starts_with ~prefix func_name then
      String.sub func_name (String.length prefix) (String.length func_name - String.length prefix)
    else
      func_name
  in

  (* Find native calls in R source files *)
  let find_calls prefix =
    let r_dir = Filename.concat path "R" in
    if not (Sys.file_exists r_dir && Sys.is_directory r_dir) then []
    else
      try
        let files = Sys.readdir r_dir in
        let calls = ref [] in
        Array.iter (fun filename ->
          let filepath = Filename.concat r_dir filename in
          if not (Sys.is_directory filepath) then
            (try
              let lines = In_channel.with_open_text filepath In_channel.input_lines in
              let content = String.concat "\n" lines in
              (* Create regex patterns for .C, .Call, .Fortran, .External *)
              let extract_calls pattern_name convention =
                let pattern = Str.regexp
                  ("\\." ^ pattern_name ^ "[ \t]*([ \t]*\"?\\([a-zA-Z0-9_]+\\)")
                in
                let rec find_all pos acc =
                  try
                    ignore (Str.search_forward pattern content pos);
                    let func_name = Str.matched_group 1 content in
                    let next_pos = Str.match_end () in
                    let native_name = strip_prefix prefix func_name in
                    find_all next_pos ((native_name, convention) :: acc)
                  with Not_found -> List.rev acc
                in
                find_all 0 []
              in
              let c_calls = extract_calls "C" C in
              let call_calls = extract_calls "Call" Call in
              let fortran_calls = extract_calls "Fortran" Fortran in
              let external_calls = extract_calls "External" External in
              calls := !calls @ c_calls @ call_calls @ fortran_calls @ external_calls
            with _ -> ())
        ) files;
        !calls
      with _ -> []
  in

  let prefix = find_prefix () in
  find_calls prefix

let get_c_files path =
  let src_dir = Filename.concat path "src" in
  if not (Sys.file_exists src_dir && Sys.is_directory src_dir) then []
  else
    try
      let files = Sys.readdir src_dir in
      Array.fold_left (fun acc filename ->
        let filepath = Filename.concat src_dir filename in
        if not (Sys.is_directory filepath) && Filename.check_suffix filename ".c" then
          filepath :: acc
        else acc
      ) [] files
    with _ -> []
