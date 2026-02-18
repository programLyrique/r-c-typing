open R_c_typing

let usage () =
  prerr_endline "Usage: package_test.exe <package-path>";
  exit 2

let () =
  if Array.length Sys.argv <> 2 then usage ();
  let package_path = Sys.argv.(1) in
  
  (* Find native calls in the package *)
  let calls = Package.find_native_calls package_path in
  
  (* Print each call *)
  List.iter (fun (func_name, calling_convention) ->
    Printf.printf "%s: %s\n" func_name calling_convention
  ) calls;
  
  (* Ensure all output is flushed before exiting *)
  flush stdout
