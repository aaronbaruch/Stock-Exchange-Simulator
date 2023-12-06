open Stocks

(* read-eval-print loop *)
(* let rec repl (eval : string -> string) : unit = print_string "> "; let input
   = read_line () in match input with | "" -> print_endline "bye" | _ -> input
   |> eval |> print_endline; repl eval *)

(*********** command line interface ***********)

let repl_string =
  "\n\
   Please enter the COMMAND you would like to execute: \n\
   \"deposit N\", \"withdraw N\", \"view_portfolio\", \"view_ledger\", \
   \"balance\", \"buy <TICKER> <SHARES>\", \"sell <TICKER> <SHARES>\",  \
   \"correlation <TICKER> <TICKER> <DAYS>\", \"news <TICKER>\", \"analytics \
   <TICKER>\", \"get_date\", or \"next_day\""

let round_to_two_decimal_places (num : float) : float =
  let multiplier = 10.0 ** 2.0 in
  Float.round (num *. multiplier) /. multiplier

let format_dollar_string (input : string) : string =
  match Float.of_string_opt input with
  | Some value ->
      let rounded_value = round_to_two_decimal_places value in
      let formatted_string = Printf.sprintf "$%.2f" rounded_value in
      formatted_string
  | None ->
      (* Invalid input, return the original string *)
      input

let rec concat_string_list (input : (string * int) list) : string =
  match input with
  | [] -> ""
  | (s, i) :: t ->
      string_of_int i ^ " shares of " ^ s ^ ", " ^ concat_string_list t

let concat_withdraw_string (input : string) =
  "You withdrew " ^ format_dollar_string input

let concat_withdraw_failure_string (input1 : string) (input2 : string) =
  "Cannot withdraw "
  ^ format_dollar_string input1
  ^ " from "
  ^ format_dollar_string input2

let concat_deposit_string (input : string) =
  "You deposited " ^ format_dollar_string input

let concat_buy_string (shares : string) (index : string) =
  "Bought" ^ shares ^ " shares of " ^ index

let concat_buy_failure_string (index : string) (shares : string)
    (balance : string) =
  "Cannot buy " ^ shares ^ " shares of " ^ index ^ ", cost more than "
  ^ format_dollar_string balance

let concat_sell_string (shares : string) (index : string) =
  "Sold " ^ shares ^ " shares of " ^ index

let concat_sell_failure_string (shares : string) (index : string)
    (port : string) =
  "Cannot sell " ^ shares ^ " shares of " ^ index ^ ". You own: " ^ port

let rec main user =
  (* Offer commands *)
  let input_is_float (n : string) =
    match float_of_string_opt n with
    | Some v -> if v >= 0. then true else false
    | None -> false
  in
  let input_is_int (n : string) =
    match int_of_string_opt n with
    | Some v -> if v >= 0 then true else false
    | None -> false
  in
  print_endline repl_string;
  print_string "> ";
  (* Check which command was made *)
  (* check for people trying to buy neg shares of stocks, and valid n, not able
     to adfgjnguihfs shares of stock *)
  match String.split_on_char ' ' (read_line ()) with
  | [ "deposit"; n ] ->
      if input_is_float n then (
        print_endline (concat_deposit_string n);
        main (Cli.Cli.deposit user (float_of_string n)))
      else print_endline "Invalid input to deposit, must be positive integer.";
      main user
  | [ "withdraw"; n ] ->
      if input_is_float n then (
        if
          Cli.Cli.view_balance user
          = Cli.Cli.view_balance (Cli.Cli.withdraw user (float_of_string n))
        then
          print_endline
            (concat_withdraw_failure_string n
               (string_of_float (Cli.Cli.view_balance user)))
        else print_endline (concat_withdraw_string n);
        main (Cli.Cli.withdraw user (float_of_string n)))
      else print_endline "Invalid input to withdraw, must be positive integer.";
      main user
  | [ "view_portfolio" ] ->
      print_endline "You have:";
      print_endline (concat_string_list (Cli.Cli.view_portfolio user));
      main user
  | [ "view_ledger" ] ->
      print_endline "Your ledger:";
      User.UserImpl.print_ledger (Cli.Cli.view_ledger user);
      main user
  | [ "buy"; x; y ] ->
      if input_is_int y then (
        match Cli.Cli.buy user x (int_of_string y) with
        | s ->
            let updated_user = s in
            if Cli.Cli.view_balance updated_user = Cli.Cli.view_balance user
            then
              print_endline
                (concat_buy_failure_string x y
                   (string_of_float (Cli.Cli.view_balance user)))
            else print_endline (concat_buy_string y x);
            main updated_user (* Use updated_user *)
        | exception Failure _ ->
            print_endline "Eror buying";
            main user)
      else print_endline "Invalid input shares to buy, must be positive integer";
      main user
  | [ "sell"; x; y ] ->
      if input_is_int y then (
        match Cli.Cli.sell user x (int_of_string y) with
        | s ->
            let updated_user = s in
            if Cli.Cli.view_balance updated_user = Cli.Cli.view_balance user
            then
              print_endline
                (concat_sell_failure_string y x
                   (concat_string_list (Cli.Cli.view_portfolio user)))
            else print_endline (concat_sell_string y x);
            main updated_user (* Use updated_user *)
        | exception Failure _ ->
            print_endline "Eror selling";
            print_endline "Going back to commands.";
            main user)
      else
        print_endline "Invalid input shares to sell, must be positive integer";
      main user
  | [ "get_date" ] ->
      let date_string = "Date: " ^ Cli.Cli.get_date_time_string user in
      print_endline date_string;
      main user
  | [ "next_day" ] ->
      let updated_user = Cli.Cli.next_day user in
      let date_string = Cli.Cli.get_date_time_string updated_user in
      print_endline ("Date: " ^ date_string);
      main updated_user
  | [ "balance" ] ->
      print_endline "You have: ";
      print_endline
        (format_dollar_string (string_of_float (Cli.Cli.view_balance user)));
      main user
  | [ "correlation"; symbol1; symbol2; days ] ->
      if input_is_int days then (
        match
          Cli.Cli.calculate_stock_correlation user symbol1 symbol2
            (int_of_string days)
        with
        | s ->
            print_endline ("Correlation: " ^ string_of_float s);
            main user
        | exception Failure _ ->
            print_endline "Eror generating stock correlation";
            print_endline "Going back to commands.";
            main user)
      else print_endline "Invalid input for days, must be positive integer";
      main user
  | [ "news"; symbol ] -> (
      match Cli.Cli.get_latest_news_feeds user symbol with
      | s ->
          print_endline ("Latest News: " ^ s);
          main user
      | exception Failure s ->
          print_endline s;
          print_endline "Going back to commands.";
          main user)
  | [ "analytics"; symbol ] -> (
      match Cli.Cli.generate_stock_summary user symbol with
      | s ->
          print_endline ("Latest Analytics: " ^ s);
          main user
      | exception Failure _ ->
          print_endline "Eror generating stock summary";
          print_endline "Going back to commands.";
          main user)
  | _ ->
      print_endline "Command not recognized";
      main user

let rec wrong_val_loop () =
  match read_float_opt () with
  | Some v -> round_to_two_decimal_places v
  | None ->
      print_endline
        "\nInvalid balance input, retry with input Integer value of money";
      wrong_val_loop ()

let () =
  print_endline "\n\nWelcome to the Market!\n";
  (* Get data *)
  print_string "> ";
  (* let file = read_line () in print_endline "Reading file..."; let input =
     file |> In_channel.open_text |> In_channel.input_all in*)
  (* Get username *)
  print_endline "Please enter your username:";
  print_string "> ";
  let username = read_line () in
  (* Get initial balance *)
  print_endline "\nHow many dollars do you want to begin with?";
  print_string "> ";
  let balance = wrong_val_loop () in
  print_endline
    "\nInput true for to use historical data and false to use live data";
  let dev_mode = bool_of_string (String.trim (read_line ())) in
  let user = ref (Cli.Cli.make_user username balance dev_mode) in
  main !user
