open Gfile
open Graph
open Tools
open Printf

let init gr =
  let temp gr arc1 = add_arc gr arc1.tgt arc1.src 0 in
  e_fold gr temp gr


let find_path gr src tgt =
  let rec dfs passed path node =
    if node = tgt then Some (List.rev (node :: path))
    else if List.mem node passed then None
    else
      let passed = node :: passed in
      let out_arcs = List.filter (fun arc -> arc.lbl > 0) (out_arcs gr node) in
      let next_nodes = List.map (fun arc -> arc.tgt) out_arcs in
      let rec explore = function
        | [] -> None
        | next_node :: rest -> (
          match dfs passed (node :: path) next_node with
          | Some path -> Some path
          | None -> explore rest
        ) in explore next_nodes
  in dfs [] [] src


let increase_flow gr path =
  let rec temp g min_capacity = function
    | [] | [_] -> (g, min_capacity)
    | src :: tgt :: rest -> (
        match find_arc g src tgt with
        | Some arc ->
            (* Tìm giá trị nhỏ nhất *)
            let new_min = min min_capacity arc.lbl in
            (* Giảm trọng số trên cung đi *)
            let updated_g = add_arc g src tgt (arc.lbl - new_min) in
            (* Tăng trọng số trên cung ngược lại *)
            let reverse_arc = find_arc updated_g tgt src in
            let updated_g =
              match reverse_arc with
              | Some r_arc -> add_arc updated_g tgt src (r_arc.lbl + new_min)
              | None -> add_arc updated_g tgt src new_min
            in temp updated_g new_min (tgt :: rest)
        | None -> failwith "Arc not found")
  in temp gr max_int path


  let ford_fulkerson gr source dest =
    let rec loop g max_flow =
      match find_path g source dest with
      | None -> max_flow (* Không tìm được đường tăng nữa *)
      | Some path ->
          (* Tìm giá trị nhỏ nhất và cập nhật dòng chảy *)
          let updated_graph, min_capacity = increase_flow g path in
          (* Tăng tổng dòng chảy *)
          loop updated_graph (max_flow + min_capacity)
    in loop gr 0  