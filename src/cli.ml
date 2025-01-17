open User
open Data

module type CliType = sig
  type stock_query = string * int

  module User_Impl = UserImpl
  module Data_Impl = DataAPI

  val make_user : string -> float -> bool -> User_Impl.t
  val deposit : User_Impl.t -> float -> User_Impl.t
  val withdraw : User_Impl.t -> float -> User_Impl.t
  val buy : User_Impl.t -> string -> int -> User_Impl.t
  val sell : User_Impl.t -> string -> int -> User_Impl.t
  val next_day : User_Impl.t -> User_Impl.t
  val get_date_time_string : User_Impl.t -> string

  (* val get_stock : User_Impl -> int -> string -> int *)
  val view_portfolio : User_Impl.t -> (string * int) list
  val view_balance : User_Impl.t -> float
  val view_ledger : User_Impl.t -> User_Impl.ledger_entry list ref

  val calculate_stock_correlation :
    User_Impl.t -> string -> string -> int -> float

  val get_latest_news_feeds : User_Impl.t -> string -> string
  val generate_stock_summary : User_Impl.t -> string -> string
end

module Cli : CliType = struct
  type stock_query = string * int

  module User_Impl = UserImpl
  module Data_Impl = DataAPI

  let make_user (username : string) (balance : float) (dev_mode : bool) :
      User_Impl.t =
    User_Impl.init_user username balance dev_mode

  let deposit (user : User_Impl.t) (n : float) = User_Impl.deposit user n
  let withdraw (user : User_Impl.t) (n : float) = User_Impl.withdraw user n

  let buy (user : User_Impl.t) (index : string) (n : int) =
    User_Impl.buy user index n

  let sell (user : User_Impl.t) (index : string) (n : int) =
    User_Impl.sell user index n

  let next_day (user : User_Impl.t) = User_Impl.next_day user

  let get_date_time_string (user : User_Impl.t) =
    User_Impl.get_date_time_string user

  (* let get_stock (index : string) = failwith "u" *)
  let view_portfolio (user : User_Impl.t) = User_Impl.portfolio user
  let view_balance (user : User_Impl.t) = User_Impl.balance user
  let view_ledger (user : User_Impl.t) = User_Impl.ledger user

  let calculate_stock_correlation (user : User_Impl.t) (symbol1 : string)
      (symbol2 : string) (lookback_days : int) =
    let day = User_Impl.get_days_back user in
    Lwt_main.run
      (Data_Impl.calculate_stock_correlation (symbol1, day) (symbol2, day)
         lookback_days)

  let news_to_string news_list =
    let news_string_of_tuple (title, summary, sentiment) =
      Printf.sprintf "Title: %s\nSummary: %s\nSentiment Score: %.2f\n\n" title
        summary sentiment
    in
    List.fold_left
      (fun acc news_item -> acc ^ news_string_of_tuple news_item)
      "" news_list

  let get_latest_news_feeds (user : User_Impl.t) (symbol : string) =
    let day = User_Impl.get_days_back user in
    news_to_string
      (Lwt_main.run (Data_Impl.get_latest_news_feeds (symbol, day)))

  let generate_stock_summary (user : User_Impl.t) (symbol : string) =
    let day = User_Impl.get_days_back user in
    Lwt_main.run (Data_Impl.generate_stock_summary (symbol, day))
end
(* module Cli : CliType = struct type t

   module User_Impl = UserImpl

   let make_user (username : string) (balance : int) : User_Impl.t =
   User_Impl.init_user username balance []

   let deposit amt = User_Impl.deposit amt let withdraw amt = User_Impl.withdraw
   amt let buy amt ticker = User_Impl.buy amt ticker let sell amt ticker =
   User_Impl.sell amt ticker let next_day = User_Impl.next_day let get_stock
   ticker = User_Impl.get_stock ticker let view_portfolio = User_Impl.portfolio
   end *)
