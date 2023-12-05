open User
open Data

module type CliType = sig
  module User_Impl = UserImpl
  module Data_Impl = DataAPI

  type stock_query = string * int

  val make_user : string -> int -> bool -> User_Impl.t
  val deposit : User_Impl.t -> int -> User_Impl.t
  val withdraw : User_Impl.t -> int -> User_Impl.t
  val buy : User_Impl.t -> string -> int -> User_Impl.t
  val sell : User_Impl.t -> string -> int -> User_Impl.t
  val next_day : User_Impl.t -> User_Impl.t

  (* val get_stock : string -> int *)
  val view_portfolio : User_Impl.t -> (string * int) list
  val view_balance : User_Impl.t -> int
  val view_ledger : User_Impl.t -> User_Impl.ledger_entry list ref

  val calculate_stock_correlation :
    User_Impl.t -> string -> string -> int -> float

  val get_latest_news_feeds : User_Impl.t -> string -> string
  val generate_stock_summary : User_Impl.t -> string -> string
end

module Cli : CliType
