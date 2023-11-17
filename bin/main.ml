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

let concat_withdraw_failure_string (input1 : string) (input2 : string) =
  "Cannot withdraw $" ^ input1 ^ " from $" ^ input2

let concat_deposit_string (input : string) = "You deposited $" ^ input

let concat_buy_string (shares : string) (index : string) =
  "Bought " ^ shares ^ " of " ^ index

let concat_buy_failure_string (index : string) (shares : string)
    (balance : string) =
  "Cannot buy " ^ shares ^ " shares of " ^ index ^ ", cost more than $"
  ^ balance

let concat_sell_string (shares : string) (index : string) =
  "Sold " ^ shares ^ " of " ^ index

let concat_sell_failure_string (shares : string) (index : string)
    (port : string) =
  "Cannot sell " ^ shares ^ " shares of " ^ index ^ ". You own: " ^ port

let rec main user =
  (* Offer commands *)
  let input_is_int (n : string) =
    match int_of_string_opt n with
    | Some v -> if v >= 0 then true else false
    | None -> false
  in
  print_endline
    "Please enter the COMMAND you would like to execute: \n\
     \"deposit N\", \"withdraw N\", \"view\", \"buy <TICKER> <SHARES>\", \
     \"sell <TICKER> <SHARES>\", \"balance\", or \"next_day\"";
  print_string "> ";

  (* Check which command was made *)
  (* check for people trying to buy neg shares of stocks, and valid n, not able
     to adfgjnguihfs shares of stock *)
  match String.split_on_char ' ' (read_line ()) with
  | [ "deposit"; n ] ->
      if input_is_int n then (
        print_endline (concat_deposit_string n);
        main (Cli.Cli.deposit user (int_of_string n)))
      else print_endline "Invalid input to deposit, must be positive integer.";
      main user
  | [ "withdraw"; n ] ->
      if input_is_int n then (
        if
          Cli.Cli.view_balance user
          = Cli.Cli.view_balance (Cli.Cli.withdraw user (int_of_string n))
        then
          print_endline
            (concat_withdraw_failure_string n
               (string_of_int (Cli.Cli.view_balance user)))
        else print_endline (concat_withdraw_string n);
        main (Cli.Cli.withdraw user (int_of_string n)))
      else print_endline "Invalid input to withdraw, must be positive integer.";
      main user
  | [ "view" ] ->
      print_endline "You have:";
      print_endline (concat_string_list (Cli.Cli.view_portfolio user));
      main user
  | [ "buy"; x; y ] ->
      if input_is_int y then (
        if
          Cli.Cli.view_balance (Cli.Cli.buy user x (int_of_string y))
          = Cli.Cli.view_balance user
        then
          print_endline
            (concat_buy_failure_string x y
               (string_of_int (Cli.Cli.view_balance user)))
        else print_endline (concat_buy_string y x);
        main (Cli.Cli.buy user x (int_of_string y)))
      else print_endline "Invalid input shares to buy, must be positive integer";
      main user
  | [ "sell"; x; y ] ->
      if input_is_int y then (
        if
          Cli.Cli.view_balance (Cli.Cli.sell user x (int_of_string y))
          = Cli.Cli.view_balance user
        then
          print_endline
            (concat_sell_failure_string y x
               (concat_string_list (Cli.Cli.view_portfolio user)))
        else print_endline (concat_sell_string y x);
        main (Cli.Cli.sell user x (int_of_string y)))
      else
        print_endline "Invalid input shares to sell, must be positive integer";
      main user
  | [ "next_day" ] ->
      print_endline "Going to the next day!";
      main (Cli.Cli.next_day user)
  | [ "balance" ] ->
      print_endline "You have: ";
      print_endline (string_of_int (Cli.Cli.view_balance user));
      main user
  | _ ->
      print_endline "Command not recognized";
      main user

let rec wrong_val_loop () =
  match read_int_opt () with
  | Some v -> v
  | None ->
      print_endline
        "\nInvalid balance input, retry with input Integer value of money";
      wrong_val_loop ()

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
  let balance = wrong_val_loop () in
  print_endline "Reading balance...";
  (* Create user *)
  let user = ref (Cli.Cli.make_user username balance) in
  print_endline "Creating user...";

  main !user
