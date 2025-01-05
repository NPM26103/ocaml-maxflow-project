open Graph
open Gfile
open Tools
open Printf

module IntMap = Map.Make(struct type t = id let compare = compare end)
module IntSet = Set.Make(struct type t = id let compare = compare end)

let init (gr: int graph) : int graph =
  let temp gr arc1 = add_arc gr arc1.tgt arc1.src 0 in
  e_fold gr temp gr

let create_flot_graph (graph: int graph) : flot graph = 
  gmap graph (fun lbl -> { capa = lbl; flot = 0 })

let arcs_residual (arc: flot arc) (graph: flot graph) : (flot arc * flot arc) =
  let capa = arc.lbl.capa in
  let flot = arc.lbl.flot in
  let residual = capa - flot in
  match find_arc graph arc.tgt arc.src with
  | None -> 
    ({ src = arc.src; tgt = arc.tgt; lbl = { capa = residual; flot = flot } },
     { src = arc.tgt; tgt = arc.src; lbl = { capa = flot; flot = 0 } })
  | Some x -> 
    ({ src = arc.src; tgt = arc.tgt; lbl = { capa = residual + x.lbl.flot; flot = flot } },
     { src = arc.tgt; tgt = arc.src; lbl = { capa = x.lbl.capa - x.lbl.flot + flot; flot = 0 } })

let graph_residual (graph: flot graph) : flot graph =
  let new_graph = clone_nodes graph in
  e_fold graph (fun g arc -> 
    let (arc1, arc2) = arcs_residual arc graph in
    new_arc (new_arc g arc1) arc2
  ) new_graph

let arc_valid (arc: flot arc) : bool =
  arc.lbl.capa > 0

let find_path_gr (graph: flot graph) (source: id) (destination: id) : id list option =
  let rec bfs queue visited parents =
    match queue with
    | [] -> None
    | node :: rest ->
      if node = destination then
        let rec build_path current acc =
          if current = source then source :: acc
          else
            match IntMap.find_opt current parents with
            | Some parent -> build_path parent (current :: acc)
            | None -> []
        in
        let path = build_path destination [] in
        Some path
      else
        let out_arcs = out_arcs graph node in
        let neighbors = 
          out_arcs
          |> List.filter arc_valid
          |> List.map (fun arc -> arc.tgt)
        in
        let new_neighbors = List.filter (fun n -> not (IntSet.mem n visited)) neighbors in
        let new_visited = List.fold_left (fun acc n -> IntSet.add n acc) visited new_neighbors in
        let new_parents = List.fold_left (fun acc n -> IntMap.add n node acc) parents new_neighbors in
        bfs (rest @ new_neighbors) new_visited new_parents
  in
  bfs [source] (IntSet.singleton source) IntMap.empty

let process_path (graph: flot graph) (path: id list) : flot graph * int =
  let min_val = 
    List.fold_left (fun acc (src, tgt) ->
      match find_arc graph src tgt with
      | Some arc -> min acc arc.lbl.capa
      | None -> acc
    ) max_int (List.combine (List.rev (List.tl (List.rev path))) (List.tl path))
    in let updated_graph =
    List.fold_left (fun g (src, tgt) ->
      match find_arc g src tgt with
      | Some arc ->
        let new_flot = arc.lbl.flot + min_val in
        add_arc g src tgt { capa = arc.lbl.capa; flot = new_flot }
      | None -> g
    ) graph (List.combine (List.rev (List.tl (List.rev path))) (List.tl path))
  in (updated_graph, min_val)

let ford_fulkerson (gr: int graph) (source: id) (destination: id) : unit =
  let initialized_graph = init gr in
  let flot_graph = create_flot_graph initialized_graph in
  let export_flot_graph (graph: flot graph) (filename: string) : unit =
    let string_of_flot arc =
      sprintf "%d/%d" arc.lbl.flot arc.lbl.capa
    in export (gmap graph (fun arc -> string_of_flot arc)) filename
  in export_flot_graph flot_graph "Graph_initial";
  
  let residual_graph = graph_residual flot_graph in
  let rec loop graph max_flow =
  match find_path_gr graph source destination with
    | None -> max_flow
    | Some path ->
      let (updated_graph, flow) = process_path graph path in
      loop updated_graph (max_flow + flow)
    in let final_flow = loop residual_graph 0 in
  
  export (gmap residual_graph (fun arc -> string_of_int arc.lbl.capa)) "Graph_residual_final";
  
  let final_graph = 
    e_fold gr (fun g arc ->
      match find_arc residual_graph arc.src arc.tgt with
      | Some res_arc -> 
        add_arc g arc.src arc.tgt (arc.lbl - res_arc.lbl.capa)
      | None -> g
    ) flot_graph
  in
  export_flot_graph final_graph "Graph_final"