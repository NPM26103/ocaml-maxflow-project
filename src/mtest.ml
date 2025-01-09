open Money_sharing

let () =
  if Array.length Sys.argv <> 2 then
    begin
      Printf.printf
        "\n ✻  Usage: %s outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  outfile : output file to write the settlement results.\n\n") ;
      exit 0
    end ;

  let outfile = Sys.argv.(1) in

  (* Define node IDs *)
  let node_ids = [0; 1; 2] in

  (* Define balances for each node
     - < 0: owes money
     - > 0: receive money *)
  let balances = [
    (0, -40);
    (1, 20);
    (2, 20);
  ] in

  let settlements = solve node_ids balances in
  let f = open_out outfile in
  Printf.fprintf f "Settlements to balance all debts:\n";
  List.iter (fun (payer, payee, amount) ->
    Printf.fprintf f "Node %d pays Node %d: %d\n" payer payee amount
  ) settlements;
  close_out f;
  ()
;;