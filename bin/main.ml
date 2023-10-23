open Stocks

(* read-eval-print loop *)
(* let rec repl (eval : string -> string) : unit = print_string "> "; let input
   = read_line () in match input with | "" -> print_endline "bye" | _ -> input
   |> eval |> print_endline; repl eval *)

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to the Market!\n";
  (* Get data *)
  print_endline "Please enter your data file";
  print_string "> ";
  (* let file = read_line () in print_endline "Reading file..."; let input =
     file |> In_channel.open_text |> In_channel.input_all in*)
  (* Get username *)
  print_endline "Please enter the your username:";
  print_string "> ";
  let username = read_line () in
  print_endline "Reading username...";
  (* Get initial balance *)
  print_endline "\n\nHow much money do you want to begin with?\n";
  print_string "> ";
  let balance = read_int () in
  print_endline "Reading balance...";
  (* Create user *)
  let user = ref (Cli.Cli.make_user username balance) in
  print_endline "Creating user...";
  (* Offer commands *)
  print_endline
    "Please enter the COMMAND you would like to execute: \n\
     \"deposit N\", \"withdraw N\", \"view\", \"buy SHARES TICKER N\", \"sell \
     SHARES TICKER N\", or \"next_day\"";
  print_string "> ";
  (* Check which command was made *)
  match String.split_on_char ' ' (read_line ()) with
  | [ "deposit"; n ] ->
      print_endline "Depositing cash";
      user := Cli.Cli.deposit !user (int_of_string n)
  | [ "withdraw"; n ] ->
      print_endline "Withdrawing cash";
      user := Cli.Cli.withdraw !user (int_of_string n)
  | [ "view" ] -> print_endline "Viewing"
  (*View portfolio later*)
  | [ "buy"; x; y ] ->
      print_endline "Buying stock";
      user := Cli.Cli.buy !user x (int_of_string y)
  | [ "sell"; x; y ] ->
      print_endline "Selling stock";
      user := Cli.Cli.sell !user x (int_of_string y)
  | [ "next_day" ] ->
      print_endline "going to next Day!";
      user := Cli.Cli.next_day !user
  | _ -> failwith "Invalid argument"
