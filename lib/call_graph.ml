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

  let dfs_from_id t start_id =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let rec visit acc id =
      if id < 0 || id >= n || visited.(id) then acc
      else (
        visited.(id) <- true;
        let acc' = id :: acc in
        List.fold_left visit acc' t.succ.(id))
    in
    List.rev (visit [] start_id)

  let dfs_from_name t name =
    match id_of_name t name with
    | None -> []
    | Some id -> dfs_from_id t id

  let bfs_from_id t start_id =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let queue = Queue.create () in
    let acc = ref [] in
    if start_id >= 0 && start_id < n then (
      visited.(start_id) <- true;
      Queue.add start_id queue);
    while not (Queue.is_empty queue) do
      let id = Queue.take queue in
      acc := id :: !acc;
      List.iter
        (fun succ_id ->
          if succ_id >= 0 && succ_id < n && not visited.(succ_id) then (
            visited.(succ_id) <- true;
            Queue.add succ_id queue))
        t.succ.(id)
    done;
    List.rev !acc

  let bfs_from_name t name =
    match id_of_name t name with
    | None -> []
    | Some id -> bfs_from_id t id

  let scc t =
    (* Kosaraju's algorithm: order by finish time, then DFS on transposed graph. *)
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let order = ref [] in
    let rec dfs1 id =
      if not visited.(id) then (
        visited.(id) <- true;
        List.iter dfs1 t.succ.(id);
        order := id :: !order)
    in
    for id = 0 to n - 1 do
      dfs1 id
    done;
    let rev_succ = Array.make n [] in
    for u = 0 to n - 1 do
      List.iter (fun v -> rev_succ.(v) <- u :: rev_succ.(v)) t.succ.(u)
    done;
    Array.fill visited 0 n false;
    let rec dfs2 acc id =
      if visited.(id) then acc
      else (
        visited.(id) <- true;
        List.fold_left dfs2 (id :: acc) rev_succ.(id))
    in
    let components = ref [] in
    List.iter
      (fun id ->
        if not visited.(id) then
          let comp = dfs2 [] id in
          components := comp :: !components)
      !order;
    List.rev !components

  let topo_sort t =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let result = ref [] in
    (* More efficient than calling the dfs_from_id multiple times as 
    we can share the visited array. *)
    let rec visit id =
      if not visited.(id) then (
        visited.(id) <- true;
        List.iter visit t.succ.(id);
        result := id :: !result)
    in
    for id = 0 to n - 1 do
      visit id
    done;
    List.rev !result

  let topo_sort_names t =
    List.filter_map (name_of_id t) (topo_sort t)

  let in_degree t =
    let n = Array.length t.id_to_name in
    let deg = Array.make n 0 in
    iter_edges t (fun _caller callee -> deg.(callee) <- deg.(callee) + 1);
    deg

  let topo_sort_kahn t =
    let deg = in_degree t in
    let queue = Queue.create () in
    Array.iteri (fun id d -> if d = 0 then Queue.add id queue) deg;
    let result = ref [] in
    while not (Queue.is_empty queue) do
      let u = Queue.take queue in
      result := u :: !result;
      List.iter
        (fun v ->
          deg.(v) <- deg.(v) - 1;
          if deg.(v) = 0 then Queue.add v queue)
        t.succ.(u)
    done;
    List.rev !result

  (** Compute all nodes reachable from a list of starting node IDs *)
  let reachable_from_ids t start_ids =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let result = ref [] in
    let rec visit id =
      if id >= 0 && id < n && not visited.(id) then (
        visited.(id) <- true;
        result := id :: !result;
        List.iter visit t.succ.(id))
    in
    List.iter visit start_ids;
    List.rev !result

  (** Compute all nodes reachable from a list of starting node names *)
  let reachable_from_names t start_names =
    let start_ids = List.filter_map (id_of_name t) start_names in
    let reachable_ids = reachable_from_ids t start_ids in
    List.filter_map (name_of_id t) reachable_ids

  (* Inline tests *)
  let%test "reachable from single node" =
    let g = create () in
    let _a = add_node g "a" in
    let _b = add_node g "b" in
    let _c = add_node g "c" in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"b" ~callee:"c";
    let reachable = reachable_from_names g ["a"] in
    reachable = ["a"; "b"; "c"]

  let%test "reachable from multiple nodes" =
    let g = create () in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"c" ~callee:"d";
    add_edge g ~caller:"b" ~callee:"e";
    let reachable = reachable_from_names g ["a"; "c"] in
    List.sort String.compare reachable = ["a"; "b"; "c"; "d"; "e"]

  let%test "reachable with cycles" =
    let g = create () in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"b" ~callee:"c";
    add_edge g ~caller:"c" ~callee:"a";
    let reachable = reachable_from_names g ["a"] in
    List.sort String.compare reachable = ["a"; "b"; "c"]

  let%test "reachable from disconnected nodes" =
    let g = create () in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"c" ~callee:"d";
    let reachable = reachable_from_names g ["x"; "y"] in
    reachable = []
end


(** Build a call graph from a single PAst top-level unit *)
let of_past_unit t (_pos, unit') =
  match unit' with
  | PAst.Fundef (_, fname, _params, body) ->
      let callees = PAst.extract_calls_from_expr body in
      List.iter (fun callee -> Callgraph.add_edge t ~caller:fname ~callee) callees
  | PAst.Struct _ -> ()

(** Build a call graph from a PAst definition (list of top-level units) *)
let of_past defs =
  let t = Callgraph.create ~capacity:(List.length defs) () in
  List.iter (of_past_unit t) defs;
  t

(** Build a call graph from multiple PAst definitions *)
let of_past_list defs_list =
  let total = List.fold_left (fun acc defs -> acc + List.length defs) 0 defs_list in
  let t = Callgraph.create ~capacity:total () in
  List.iter (List.iter (of_past_unit t)) defs_list;
  t

let keep_reachable t entry_points =
  let reachable_names = Callgraph.reachable_from_names t entry_points in
  let module StrSet = Set.Make(String) in
  let reachable_set = StrSet.of_list reachable_names in
  let new_graph = Callgraph.create ~capacity:(List.length reachable_names) () in
  (* Add edges only between reachable nodes *)
  List.iter (fun caller ->
    let callees = Callgraph.successors_by_name t caller in
    List.iter (fun callee ->
      if StrSet.mem callee reachable_set then
        Callgraph.add_edge new_graph ~caller ~callee
    ) callees
  ) reachable_names;
  new_graph

let topo_sort pasts call_graph =
  let past_map = Hashtbl.create (List.length pasts) in
  List.iter (fun (filename, past) ->
    match past with
      | _, PAst.Fundef (_, name, _, _) -> Hashtbl.add past_map name (filename, past)
      | _, PAst.Struct _ -> ()
  ) pasts;
  let sorted_names = Callgraph.topo_sort_names call_graph in
  List.filter_map (fun name -> Hashtbl.find_opt past_map name) sorted_names


(* Inline tests *)

(* Inline test for keep_reachable *)
let%test "keep_reachable filters unreachable nodes" =
  let g = Callgraph.create () in
  Callgraph.add_edge g ~caller:"main" ~callee:"helper1";
  Callgraph.add_edge g ~caller:"helper1" ~callee:"util";
  Callgraph.add_edge g ~caller:"unused" ~callee:"dead_code";
  let filtered = keep_reachable g ["main"] in
  Callgraph.node_count filtered = 3 &&
  Callgraph.has_node filtered "main" &&
  Callgraph.has_node filtered "helper1" &&
  Callgraph.has_node filtered "util" &&
  not (Callgraph.has_node filtered "unused") &&
  not (Callgraph.has_node filtered "dead_code")

let%test "keep_reachable preserves edges" =
  let g = Callgraph.create () in
  Callgraph.add_edge g ~caller:"a" ~callee:"b";
  Callgraph.add_edge g ~caller:"b" ~callee:"c";
  let filtered = keep_reachable g ["a"] in
  Callgraph.successors_by_name filtered "a" = ["b"] &&
  Callgraph.successors_by_name filtered "b" = ["c"]

let%test "keep_reachable with multiple entry points" =
  let g = Callgraph.create () in
  Callgraph.add_edge g ~caller:"main1" ~callee:"shared";
  Callgraph.add_edge g ~caller:"main2" ~callee:"shared";
  Callgraph.add_edge g ~caller:"unused" ~callee:"dead";
  let filtered = keep_reachable g ["main1"; "main2"] in
  Callgraph.node_count filtered = 3 &&
  not (Callgraph.has_node filtered "unused")


