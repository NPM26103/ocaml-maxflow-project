open Graph
open Tools

type paths = id list

let rec find_path gr visited id1 id2 =
  if id1 = id2 then Some [id1]
  else
    let out_arcs_node = out_arcs gr id1 in

    let rec is_visited x = function
      | [] -> false
      | a :: _ when x = a -> true
      | _ :: rest -> is_visited x rest
    in

    let rec arcs_route = function
      | [] -> []
      | { src = _; tgt = dst; lbl = vl } :: rest ->
          if is_visited dst visited || vl <= 0 then arcs_route rest
          else dst :: arcs_route rest
    in
    match arcs_route out_arcs_node with
    | [] -> None
    | x :: _ ->
        match find_path gr (x :: visited) x id2 with
        | None -> find_path gr (x :: visited) id1 id2
        | Some liste -> Some (id1 :: liste)

;;

let rec max_flow_path gr max = function
  | [] -> max
  | _ :: [] -> max
  | nd1 :: nd2 :: rest ->
      match find_arc gr nd1 nd2 with
      | None -> 0
      | Some arc -> max_flow_path gr (min max arc.lbl) (nd2 :: rest)
;;

let rec modify_flow gr n = function
  | [] -> gr
  | _ :: [] -> gr
  | nd1 :: nd2 :: rest -> modify_flow (add_arc gr nd1 nd2 (-n)) n (nd2 :: rest)
;;

let inter_residuel_graph grapho resd_graph =
  let graphn = clone_nodes grapho in
  let f graphvide { src = id1; tgt = id2; lbl = id } =
    let v =
      match find_arc resd_graph id1 id2 with
      | None -> raise (Failure "Error")
      | Some z -> z
    in
    new_arc graphvide { src = id1; tgt = id2; lbl = (string_of_int (id - v.lbl) ^ "/" ^ string_of_int id) }
  in
  e_fold grapho f graphn
;;

let ford_fulkerson graph id1 id2 =
  let rec loop l_graph id1 id2 =
    let max = 500 in
    let visited = [] in
    let path = find_path l_graph visited id1 id2 in

    match path with
    | None -> inter_residuel_graph graph l_graph
    | Some list ->
        let flow = max_flow_path l_graph max list in
        let modified_resid_graph = modify_flow l_graph flow list in
        loop modified_resid_graph id1 id2
  in
  loop graph id1 id2
;;
