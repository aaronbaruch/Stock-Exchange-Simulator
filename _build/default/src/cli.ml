Open User
Open Data

module type CliType = sig
  type t

  val make_user : string -> int -> User.t
  val deposit : int -> bool
  val withdraw : int -> bool
  val buy : int -> string -> bool
  val sell : int -> string -> bool
  val next_day : Data.t
  val get_stock : string -> int
  val view_portfolio : User.t
end

module Cli : CliType = struct
  type t

  let make_user (username : string) (balance : int) : User.t =
    User.build username balance []

  let deposit amt = User.deposit amt
  let withdraw amt = User.withdraw amt
  let buy amt ticker = User.buy amt ticker
  let sell amt ticker = User.sell amt ticker
  let next_day = User.next_day
  let get_stock ticker = User.get_stock ticker
  let view_portfolio = User.portfolio
end
