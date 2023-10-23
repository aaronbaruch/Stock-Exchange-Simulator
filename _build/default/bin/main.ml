open Stocks

(* read-eval-print loop *)
(* let rec repl (eval : string -> string) : unit = print_string "> "; let input
   = read_line () in match input with | "" -> print_endline "bye" | _ -> input
   |> eval |> print_endline; repl eval *)

(*********** command line interface ***********)
let rec concat_string_list (input : (string * int) list) : string =
  match input with
  | [] -> ""
  | (s, i) :: t ->
      string_of_int i ^ " shares of " ^ s ^ ", " ^ concat_string_list t

let concat_withdraw_string (input : string) = "You withdrew $" ^ input
let concat_deposit_string (input : string) = "You deposited $" ^ input

let concat_buy_string (shares : string) (index : string) =
  "Bought " ^ shares ^ " of " ^ index

let concat_sell_string (shares : string) (index : string) =
  "Sold " ^ shares ^ " of " ^ index

let rec main user =
  (* Offer commands *)
  print_endline
    "Please enter the COMMAND you would like to execute: \n\
     \"deposit N\", \"withdraw N\", \"view\", \"buy <TICKER> <SHARES>\", \
     \"sell <TICKER> <SHARES>\", \"balance\", or \"next_day\"";
  print_string "> ";

  (* Check which command was made *)
  (* TODO: Check if user has balances (meet qualifications) and give different
     messages *)
  match String.split_on_char ' ' (read_line ()) with
  | [ "deposit"; n ] ->
      print_endline (concat_deposit_string n);
      main (Cli.Cli.deposit user (int_of_string n))
  | [ "withdraw"; n ] ->
      print_endline (concat_withdraw_string n);
      main (Cli.Cli.withdraw user (int_of_string n))
  | [ "view" ] ->
      print_endline "You have:";
      print_endline (concat_string_list (Cli.Cli.view_portfolio user));
      main user
  | [ "buy"; x; y ] ->
      print_endline (concat_buy_string y x);
      main (Cli.Cli.buy user x (int_of_string y))
  | [ "sell"; x; y ] ->
      print_endline (concat_sell_string y x);
      main (Cli.Cli.sell user x (int_of_string y))
  | [ "next_day" ] ->
      print_endline "Going to the next day!";
      main (Cli.Cli.next_day user)
  | [ "balance" ] ->
      print_endline "You have: ";
      print_endline (string_of_int (Cli.Cli.view_balance user));
      main user
  | _ -> failwith "Invalid argument"

let () =
  print_endline "\n\nWelcome to the Market!\n";
  (* Get data *)
  print_endline "Please enter your data file";
  print_string "> ";
  (* let file = read_line () in print_endline "Reading file..."; let input =
     file |> In_channel.open_text |> In_channel.input_all in*)
  (* Get username *)
  print_endline "Please enter your username:";
  print_string "> ";
  let username = read_line () in
  print_endline "Reading username...";
  (* Get initial balance *)
  print_endline "\nHow much money do you want to begin with?";
  print_string "> ";
  let balance = read_int () in
  print_endline "Reading balance...";
  (* Create user *)
  let user = ref (Cli.Cli.make_user username balance) in
  print_endline "Creating user...";

  main !user
