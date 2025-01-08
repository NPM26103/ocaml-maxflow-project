open Graph

let clone_nodes gr = 
  n_fold gr (fun gr' id -> new_node gr' id) empty_graph

let gmap gr f =
  let new_graph = clone_nodes gr in
  e_fold gr (fun g arc -> new_arc g {src=arc.src; tgt=arc.tgt; lbl = (f arc.lbl)}) new_graph

  let add_arc g id1 id2 n =
    let arc_found = find_arc g id1 id2 in
    match arc_found with
    | Some arc -> new_arc g {src = id1; tgt = id2; lbl = arc.lbl + n}
    | None -> new_arc g {src = id1; tgt = id2; lbl = n}