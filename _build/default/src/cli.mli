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

  (* val get_stock : string -> int *)
  val view_portfolio : User_Impl.t -> (string * int) list
  val view_balance : User_Impl.t -> int
  val view_ledger : User_Impl.t -> User_Impl.ledger_entry list ref
  val calculate_stock_correlation : string -> string -> int -> float
end

module Cli : CliType
