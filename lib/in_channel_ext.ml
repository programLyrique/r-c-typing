include In_channel  (* stdlib's In_channel *)

let of_string (s : string) : in_channel =
  let (read_fd, write_fd) = Unix.pipe () in
  let oc = Unix.out_channel_of_descr write_fd in
  output_string oc s;
  flush oc;
  close_out oc;
  Unix.in_channel_of_descr read_fd

let with_string (s : string) ~(f : in_channel -> 'a) : 'a =
  let ic = of_string s in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> f ic)

let iter_lines ic ~f =
  let rec loop () =
    match input_line ic with
    | Some line ->
        f line;
        loop ()
    | None -> ()
  in
  loop ()

let fold_lines ic ~init ~f =
  let rec loop acc =
    match input_line ic with
    | Some line -> loop (f acc line)
    | None -> acc
  in
  loop init