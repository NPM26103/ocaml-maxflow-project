open Graph
open Gfile
open Printf

type flot = { 
  capa : int;
  flot : int
}

module IntMap = Map.Make(struct type t = id let compare = compare end)
module IntSet = Set.Make(struct type t = id let compare = compare end)

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

let add_flot_arc (gr: flot graph) (id1: id) (id2: id) (f: flot) : flot graph =
  match find_arc gr id1 id2 with
  | Some x ->
      let updated_flot = {capa = x.lbl.capa; flot = x.lbl.flot + f.flot} in
      new_arc gr { src = x.src; tgt = x.tgt; lbl = updated_flot }
  | None -> new_arc gr {src = id1; tgt = id2; lbl = f}

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
        add_flot_arc g src tgt {capa = arc.lbl.capa; flot = new_flot}
      | None -> g
    ) graph (List.combine (List.rev (List.tl (List.rev path))) (List.tl path))
  in (updated_graph, min_val)

  let ford_fulkerson (gr : int graph) (source : id) (destination : id) : flot graph * int =
    let flot_graph = create_flot_graph gr in
    let rec loop graph total_flow =
      match find_path_gr graph source destination with
      | None -> (graph, total_flow)
      | Some path ->
          let (updated_graph, flow) = process_path graph path in
          loop updated_graph (total_flow + flow)
    in loop flot_graph 0