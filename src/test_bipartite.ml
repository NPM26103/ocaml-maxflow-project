open Bipartite_matching

let () =
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s input_file output_file\n\n%s%!" Sys.argv.(0)
        ("    🟄  input_file  : Path to the input text file containing workers, jobs, and qualifications.\n" ^
         "    🟄  output_file : Path to the output text file where the matching results will be written.\n\n") ;
      exit 0
    end
  else
    let input_file = Sys.argv.(1) in
    let output_file = Sys.argv.(2) in

    let bg = read_input input_file in

    let flow_network = construct_flow_network bg in

    let source = -1 in
    let sink = -2 in

    let matching = find_max_matching flow_network source sink bg.workers bg.jobs in

    export_matching matching output_file;

    Printf.printf "Maximum Matching:\n";
    List.iter (fun (w, j) ->
      Printf.printf "Worker %d is assigned to Job %d\n" w j) matching