open Graph
open Tools

type paths = id list

(* Find a path using BFS *)
let find_path gr id1 id2 =
  let module Queue = Queue in
  let q = Queue.create () in
  Queue.add ([id1], id1) q;
  
  let rec bfs () =
    if Queue.is_empty q then
      None
    else
      let (path, last) = Queue.take q in
      if last = id2 then
        Some path (* Path to destination found *)
      else begin
        let out_arcs_node = out_arcs gr last in
        (* Explore all outgoing arcs from the current node *)
        List.iter (fun arc ->
          let dst = arc.tgt in
          if not (List.mem dst path) && arc.lbl > 0 then
            Queue.add (path @ [dst], dst) q (* Enqueue the new path *)
        ) out_arcs_node;
        bfs ()
      end
  in
  bfs ()

;;

(* Find the minimum capacity along the given path *)
let rec max_flow_path gr max_capacity = function
  | [] -> max_capacity
  | _ :: [] -> max_capacity
  | nd1 :: nd2 :: rest ->
      match find_arc gr nd1 nd2 with
      | None -> 0
      | Some arc -> 
          let new_max = min max_capacity arc.lbl in
          max_flow_path gr new_max (nd2 :: rest)
;;

(* Update the flow in both forward and reverse directions along the path *)
let rec modify_flow gr flow = function
  | [] -> gr
  | _ :: [] -> gr
  | nd1 :: nd2 :: rest ->
      (* Decrease capacity on forward arc and increase on reverse arc *)
      let gr_updated = add_arc (add_arc gr nd1 nd2 (-flow)) nd2 nd1 flow in
      modify_flow gr_updated flow (nd2 :: rest)
;;

(* Create the residual graph based on the current residual capacities *)
let inter_residuel_graph original_graph residual_graph =
  let residual = clone_nodes original_graph in
  (* Update residual capacities for each arc *)
  let update_residual graph_vid arc =
    let { src; tgt; lbl = id } = arc in
    match find_arc residual_graph src tgt with
    | None -> raise (Failure "Residual arc not found")
    | Some res_arc ->
        let updated_lbl = string_of_int (id - res_arc.lbl) ^ "/" ^ string_of_int id in
        new_arc graph_vid { src; tgt; lbl = updated_lbl }
  in
  e_fold original_graph update_residual residual
;;

(* Implement the Ford-Fulkerson algorithm to compute maximum flow *)
let ford_fulkerson graph source sink =
  let rec loop current_graph =
    (* Attempt to find an augmenting path using BFS *)
    match find_path current_graph source sink with
    | None -> 
        (* No more augmenting paths found, return the residual graph *)
        inter_residuel_graph graph current_graph
    | Some path ->
        (* Determine the possible flow through the found path *)
        let flow = max_flow_path current_graph 500 path in
        (* Update the residual graph with the new flow *)
        let updated_graph = modify_flow current_graph flow path in
        loop updated_graph
  in
  loop graph
;;