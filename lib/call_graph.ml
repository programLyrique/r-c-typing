(** Generate a call graph of the functions from the PAst

But maybe it is better rather do a dependence graph of the includes in C. That should be enough. 
The interesting feature of the call graph is that we can filter out calls that are not defined
*)

module Callgraph = struct
  type t = {
    name_to_id: (string, int) Hashtbl.t;
    mutable id_to_name: string array;
    mutable succ: int list array;
  }

  let create ?(capacity = 16) () =
    {
      name_to_id = Hashtbl.create capacity;
      id_to_name = Array.make 0 "";
      succ = Array.make 0 [];
    }

  let add_node t name =
    match Hashtbl.find_opt t.name_to_id name with
    | Some id -> id
    | None ->
        let id = Array.length t.id_to_name in
        Hashtbl.add t.name_to_id name id;
        t.id_to_name <- Array.append t.id_to_name [| name |];
        t.succ <- Array.append t.succ [| [] |];
        id

  let add_edge t ~caller ~callee =
    let caller_id = add_node t caller in
    let callee_id = add_node t callee in
    t.succ.(caller_id) <- callee_id :: t.succ.(caller_id)

  let of_adjacency edges =
    let g = create ~capacity:(List.length edges) () in
    List.iter
      (fun (caller, callees) ->
        let caller_id = add_node g caller in
        let callee_ids = List.map (add_node g) callees in
        g.succ.(caller_id) <- List.rev_append callee_ids g.succ.(caller_id))
      edges;
    g

  let node_count t = Array.length t.id_to_name

  let has_node t name = Hashtbl.mem t.name_to_id name

  let id_of_name t name = Hashtbl.find_opt t.name_to_id name

  let name_of_id t id =
    if id < 0 || id >= Array.length t.id_to_name then None
    else Some t.id_to_name.(id)

  let successors t id =
    if id < 0 || id >= Array.length t.succ then [] else t.succ.(id)

  let successors_by_name t name =
    match id_of_name t name with
    | None -> []
    | Some id ->
        List.filter_map (name_of_id t) (successors t id)

  let iter_nodes t f = Array.iter f t.id_to_name

  let iter_edges t f =
    Array.iteri
      (fun caller_id callees ->
        List.iter (fun callee_id -> f caller_id callee_id) callees)
      t.succ

  let fold_edges t init f =
    let acc = ref init in
    iter_edges t (fun caller_id callee_id -> acc := f !acc caller_id callee_id);
    !acc
end

