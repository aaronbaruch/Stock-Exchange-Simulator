open Data

open Lwt
(** Module defines User and functions to interact with a financial system*)

module type User = sig
  type action
  type ledger_entry

  type t
  (** Type representing user*)

  val init_user : string -> float -> bool -> t
  (** Initialize User *)

  val deposit : t -> float -> t
  (** Deposit money into user's account *)

  val withdraw : t -> float -> t
  (** Withdraw money from user's account *)

  val balance : t -> float
  (** Check balance of user's account*)

  val portfolio : t -> (string * int) list
  (** Check Portfolio of user's account*)

  val ledger : t -> ledger_entry list ref

  val sell : t -> string -> int -> t
  (** Sell stocks from User's portfolio *)

  val buy : t -> string -> int -> t
  (** Buy stocks for User's portfolio *)

  val next_day : t -> t
  (** Move the user to the next day *)
  val display_username : t -> string
  val get_day : t -> int
  (** Gets the current day of the user *)
  val print_ledger : ledger_entry list ref -> unit
end

(** Implementation of User *)
module UserImpl : User = struct
  type action =
    | Buy of
        string * int * float (* Ticker, Number of shares, Price per share *)
    | Sell of
        string * int * float (* Ticker, Number of shares, Price per share *)
    | Deposit of float
    | Withdraw of float

  type ledger_entry = {
    date : string;
    action : action;
    balance : float;
  }

  type t = {
    username : string;
    balance : float;
    stocks : (string * int) list;
    days_back : int;
    ledger : ledger_entry list ref;
  }

  module DataAPI = DataAPI

  (** Type representation of User, represent's a user's critical information *)

  (** [init_user username balance] creates a new user account with the given
      [username] and initial [balance]. This user starts with an empty portfolio
      and day 0. *)

let init_user (username : string) (balance : float) (dev_mode : bool) : t =
    let n = if dev_mode then -1 else 100 in
    { username; balance; stocks = []; days_back = n; ledger = ref [] }

  let update_balance (user : t) (n : float) = user.balance +. n

  (** [deposit user n] increases the user's balance by [n] dollars *)
  let deposit (user : t) (n : float) =
    let entry =
      {
        date = DataAPI.get_date ();
        action = Deposit n;
        balance = update_balance user n;
      }
    in
    user.ledger := entry :: !(user.ledger);
    { user with balance = update_balance user n }

  (** [withdraw user n] decreases the user's balance by [n] dollars *)
  let withdraw (user : t) (n : float) =
    let entry =
      {
        date = DataAPI.get_date ();
        action = Withdraw n;
        balance = update_balance user (-1. *. n);
      }
    in
    user.ledger := entry :: !(user.ledger);
    { user with balance = update_balance user (-1. *. n) }

  (** [balance user] returns an int of the user's balance *)
  let balance (user : t) : float = user.balance

  (** [balance user] returns an (string,int) list of the user's portfolio. *)
  let portfolio (user : t) : (string * int) list = user.stocks

  let ledger (user : t) = user.ledger

  let able_to_buy (user : t) (symbol : string) (n : int) : bool =
    let day = user.days_back in
    let ticker_price =
      Lwt_main.run
        ( DataAPI.get_ticker_price (symbol, day) >>= fun ticker_price_str ->
          Lwt.return (float_of_string ticker_price_str) )

    in
    (* Debug print statement Printf.printf "Ticker Price: %d\n" ticker_price;
       flush stdout;*)
    ticker_price *. float_of_int n <= user.balance

  let rec update_user_stocks_list (stocks : (string * int) list)
      (index : string) (n : int) : (string * int) list =
    match stocks with
    | [] -> stocks
    | (k, m) :: t ->
        if k = index then (k, m + n) :: update_user_stocks_list t index n
        else (k, m) :: update_user_stocks_list t index n

  let update_new_stock_list (stocks : (string * int) list) (index : string)
      (n : int) : (string * int) list =
    let user_stocks = update_user_stocks_list stocks index n in
    if user_stocks = stocks then (index, n) :: stocks else user_stocks

  (** [buy user index n] user [user] buys [n] shares of a stock of index
      [index]. It deducts the cost from the user's balance and updates their
      stock portfolio. Returns either the original [user] as the input if the
      stock could not be bought. Returns the edited user with bought stock if
      the stock could be bought.*)
  let buy (user : t) (index : string) (n : int) =
    let day = user.days_back in
    if able_to_buy user index n then (
      let ticker_price =
        Lwt_main.run
          ( DataAPI.get_ticker_price (index, day) >>= fun ticker_price_str ->
            Lwt.return (float_of_string ticker_price_str) )
      in
      let cost = ticker_price *. float_of_int n in
      let entry =
        {
          date = DataAPI.get_date ();
          action = Buy (index, n, ticker_price);
          balance = update_balance user (-1. *. cost);
        }
      in
      user.ledger := entry :: !(user.ledger);
      {
        user with
        stocks = update_new_stock_list user.stocks index n;
        balance = update_balance user (-1. *. cost);
      })
    else user

  let rec able_to_sell (stocks : (string * int) list) (index : string) (n : int)
      : bool =
    match stocks with
    | [] -> false
    | (_, i) :: t -> if i >= n then true else able_to_sell t index n

  (** [sell user index n] user [user] sells [n] shares of a stock of index
      [index]. It adds to the user's balance and updates their stock portfolio.
      Returns either the original [user] as the input if the stock could not be
      sold (trying to sell stock they don't have). Returns the edited user with
      sold stock if the stock could be sold.*)
  let sell (user : t) (index : string) (n : int) =
    if able_to_sell user.stocks index n = true then (
      let ticker_price =
        Lwt_main.run
          ( DataAPI.get_ticker_price (index, user.days_back)
          >>= fun ticker_price_str ->
            Lwt.return (float_of_string ticker_price_str) )
      in
      let revenue = ticker_price *. float_of_int n in
      let entry =
        {
          date = DataAPI.get_date ();
          action = Sell (index, n, ticker_price);
          balance = update_balance user revenue;
        }
      in
      user.ledger := entry :: !(user.ledger);
      {
        user with
        stocks = update_user_stocks_list user.stocks index (-1 * n);
        balance = update_balance user revenue;
      })
    else user

  (** Iterates the user by 1 to the next day*)
  let next_day (user : t) = { user with days_back = user.days_back + 1 }

  let display_username (user : t) : string = user.username
  let get_day (user : t) : int = user.days_back

  let print_ledger (ledger : ledger_entry list ref) : unit =
    List.iter
      (fun entry ->
        match entry.action with
        | Buy (ticker, shares, price) ->
            Printf.printf
              "Date: %s, Buy %d shares of %s at $%.2f, Balance: $%.2f\n"
              entry.date shares ticker price entry.balance
        | Sell (ticker, shares, price) ->
            Printf.printf
              "Date: %s, Sell %d shares of %s at $%.2f, Balance: $%.2f\n"
              entry.date shares ticker price entry.balance
        | Deposit amount ->
            Printf.printf "Date: %s, Deposit $%.2f, Balance: $%.2f\n" entry.date
              amount entry.balance
        | Withdraw amount ->
            Printf.printf "Date: %s, Withdraw $%.2f, Balance: $%.2f\n"
              entry.date amount entry.balance)
      !ledger
end