open User
open Data

module type CliType = sig
  module User_Impl = UserImpl
  module Data_Impl = DataAPI

  val make_user : string -> int -> User_Impl.t
  val deposit : User_Impl.t -> int -> User_Impl.t
  val withdraw : User_Impl.t -> int -> User_Impl.t
  val buy : User_Impl.t -> string -> int -> User_Impl.t
  val sell : User_Impl.t -> string -> int -> User_Impl.t
  val next_day : User_Impl.t -> User_Impl.t

  (* val get_stock : User_Impl -> int -> string -> int *)
  val view_portfolio : User_Impl.t -> (string * int) list
  val view_balance : User_Impl.t -> int
  val view_ledger : User_Impl.t -> User_Impl.ledger_entry list ref
  val calculate_stock_correlation : string -> string -> int -> float
  val get_latest_news_feeds : string -> string
end

module Cli : CliType = struct
  module User_Impl = UserImpl
  module Data_Impl = DataAPI

  let make_user (username : string) (balance : int) : User_Impl.t =
    User_Impl.init_user username balance

  let deposit (user : User_Impl.t) (n : int) = User_Impl.deposit user n
  let withdraw (user : User_Impl.t) (n : int) = User_Impl.withdraw user n

  let buy (user : User_Impl.t) (index : string) (n : int) =
    User_Impl.buy user index n

  let sell (user : User_Impl.t) (index : string) (n : int) =
    User_Impl.sell user index n

  let next_day (user : User_Impl.t) = User_Impl.next_day user

  (* let get_stock (index : string) = failwith "u" *)
  let view_portfolio (user : User_Impl.t) = User_Impl.portfolio user
  let view_balance (user : User_Impl.t) = User_Impl.balance user
  let view_ledger (user : User_Impl.t) = User_Impl.ledger user

  let calculate_stock_correlation (symbol1 : string) (symbol2 : string)
      (days : int) =
    Lwt_main.run (Data_Impl.calculate_stock_correlation symbol1 symbol2 days)

  let news_to_string news_list =
    let news_string_of_tuple (title, summary, sentiment) =
      Printf.sprintf "Title: %s\nSummary: %s\nSentiment Score: %.2f\n\n" title
        summary sentiment
    in
    List.fold_left
      (fun acc news_item -> acc ^ news_string_of_tuple news_item)
      "" news_list

  let get_latest_news_feeds (symbol : string) =
    news_to_string (Lwt_main.run (Data_Impl.get_latest_news_feeds symbol))
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
