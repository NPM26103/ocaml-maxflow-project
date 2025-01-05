open Graph
open Printf
open Ford_fulkerson

type worker = int
type job = int

type bipartite_graph = {
  workers : worker list;
  jobs : job list;
  qualifications : (worker * job) list;
}

type matching = (worker * job) list

let read_input (filename : string) : bipartite_graph =
  let infile = open_in filename in
  let rec read_sections workers jobs qualifications current_section =
    try
      let line = input_line infile |> String.trim in
      if line = "" then
        read_sections workers jobs qualifications current_section
      else if String.length line >= 2 && String.sub line 0 2 = "%%" then
        let section = String.trim (String.sub line 2 (String.length line - 2)) in
        read_sections workers jobs qualifications section
      else
        match current_section with
        | "Workers" ->
            let w = int_of_string line in
            read_sections (w :: workers) jobs qualifications current_section
        | "Jobs" ->
            let j = int_of_string line in
            read_sections workers (j :: jobs) qualifications current_section
        | "Qualifications" ->
            (match String.split_on_char ' ' line with
            | [w_str; j_str] ->
                let w = int_of_string w_str in
                let j = int_of_string j_str in
                read_sections workers jobs ((w, j) :: qualifications) current_section
            | _ ->
                Printf.printf "Invalid qualification line: %s\n" line;
                failwith "Invalid qualification format")
        | _ ->
            Printf.printf "Unknown section or invalid line: %s\n" line;
            failwith "Invalid section"
    with
    | End_of_file ->
        close_in infile;
        { workers = List.rev workers; jobs = List.rev jobs; qualifications = List.rev qualifications }
    | Failure msg ->
        Printf.printf "Error parsing file: %s\n" msg;
        close_in infile;
        failwith "Parsing error"
  in
  read_sections [] [] [] "" 

let construct_flow_network (bg : bipartite_graph) : flot graph =
  let gr = empty_graph in
  let source = -1 in
  let sink = -2 in
  let gr = new_node gr source in
  let gr = new_node gr sink in

  let gr = List.fold_left (fun acc w -> new_node acc w) gr bg.workers in
  let gr = List.fold_left (fun acc j -> new_node acc j) gr bg.jobs in

  (* Add edges from source to workers with capacity 1 *)
  let gr = List.fold_left (fun acc w ->
    new_arc acc { src = source; tgt = w; lbl = { capa = 1; flot = 0 } }
  ) gr bg.workers in

  (* Add edges from jobs to sink with capacity 1 *)
  let gr = List.fold_left (fun acc j ->
    new_arc acc { src = j; tgt = sink; lbl = { capa = 1; flot = 0 } }
  ) gr bg.jobs in

  (* Add edges between workers and jobs with capacity 1 based on qualifications *)
  let gr = List.fold_left (fun acc (w, j) ->
    new_arc acc { src = w; tgt = j; lbl = { capa = 1; flot = 0 } }
  ) gr bg.qualifications in

  gr

let find_max_matching (flow_network : flot graph) (source : id) (sink : id) (workers : worker list) (jobs : job list) : matching =
  let (final_flow_graph, _) = ford_fulkerson flow_network source sink in

  (* Extract matching from the final flow graph *)
  let matching = ref [] in
  List.iter (fun w ->
    try
      let arcs = out_arcs final_flow_graph w in
      List.iter (fun arc ->
        if arc.lbl.flot > 0 then
          matching := (w, arc.tgt) :: !matching
      ) arcs
    with Graph_error _ -> ()
  ) workers;
  !matching

let export_matching (matching : matching) (filename : string) : unit =
  let outfile = open_out filename in
  List.iter (fun (w, j) ->
    fprintf outfile "Worker %d is assigned to Job %d\n" w j
  ) matching;
  close_out outfile