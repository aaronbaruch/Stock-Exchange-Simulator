open Cli
open Stock
open User

(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      input |> eval |> print_endline;
      repl eval

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to the Market!\n";
  (* Get data *)
  print_endline "Please enter your data file";
  print_string "> ";
  let file = read_line () in 
  print_endline "Reading file...";
  let input = file |> In_channel.open_text |> In_channel.input_all in
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
  let user = Cli.make_user username balance in 
  print_endline "Creating user...";
  (* Offer commands *)
  print_endline "Please enter the COMMAND you would like to execute: \n\
                \"deposit N\", \"withdraw N\", \"view\", \
                \"buy SHARES TICKER N\", \"sell SHARES TICKER N\", \
                or \"next_day\"";
  print_string "> ";
  (* Check which command was made *)
  match String.split_on_char ' ' (read_line ()) with
  | ["deposit" ; n] ->
  | ["withdraw" ; n] ->
  | ["view"] ->
  | ["buy" ; x ; y] ->
  | ["sell" ; x ; y] ->
  | ["next_day"] ->
  | _ -> failwith "Invalid argument"