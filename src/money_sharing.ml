open Graph
open Ford_fulkerson
open Tools

(* Create graph nodes from a list of integer IDs *)
let create_nodes id_list =
  let initial_graph = empty_graph in
  let rec add_nodes graph ids =
    match ids with
    | [] -> graph
    | id :: rest -> add_nodes (new_node graph id) rest
  in
  add_nodes initial_graph id_list
;;

(* Connect all nodes with bidirectional arcs of capacity 100.
  This creates a fully connected graph where each node is connected to every other node.
*)
let connect_all_nodes graph id_list =
  let rec connect_single_node graph src_id targets =
    match targets with
    | [] -> graph
    | tgt_id :: rest ->
        let graph_with_arc = add_arc graph src_id tgt_id 100 in
        let graph_with_reversed_arc = add_arc graph_with_arc tgt_id src_id 100 in
        connect_single_node graph_with_reversed_arc src_id rest
  in
  let rec connect_nodes_rec graph nodes =
    match nodes with
    | [] -> graph
    | src_id :: rest ->
        let updated_graph = connect_single_node graph src_id rest in
        connect_nodes_rec updated_graph rest
  in
  connect_nodes_rec graph id_list
;;

(* 
  Create the final graph by adding a super source (2000) and super sink (1000).
  It connects the super source to debtors and creditors to the super sink based on the balance list.
  - If the balance <= 0 -> connects the super source to the node with capacity equal to the absolute balance.
  - If the balance > 0 -> connects the node to the super sink with capacity equal to the balance.
*)
let create_final_graph graph balance_list =
  let graph_with_super_nodes = new_node (new_node graph 2000) 1000 in
  
  let rec add_balances graph balances =
    match balances with
    | [] -> graph
    | (id, balance) :: rest ->
        if balance <= 0 then
          (* Debtor: Connect super source (2000) to debtor node *)
          let updated_graph = new_arc graph { src = 2000; tgt = id; lbl = (-balance) } in
          add_balances updated_graph rest
        else
          (* Creditor: Connect creditor node to super sink (1000) *)
          let updated_graph = new_arc graph { src = id; tgt = 1000; lbl = balance } in
          add_balances updated_graph rest
  in
  add_balances graph_with_super_nodes balance_list
;;

(* Extract settlements from the residual graph after applying Ford-Fulkerson *)
let extract_settlements residual_graph =
  let settlements = ref [] in
  e_iter residual_graph (fun arc ->
    if arc.src <> 2000 && arc.tgt <> 1000 then
      let payer = arc.src in
      let payee = arc.tgt in
      let amount = int_of_string arc.lbl in
      if amount > 0 then
        settlements := (payer, payee, amount) :: !settlements
  );
  !settlements
;;

let solve node_ids balances =
  let initial_graph = create_nodes node_ids in
  let connected_graph = connect_all_nodes initial_graph node_ids in
  let final_graph = create_final_graph connected_graph balances in
  let residual_graph = ford_fulkerson final_graph 2000 1000 in
  extract_settlements residual_graph
;;
