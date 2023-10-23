open User

module type CliType = sig
  module User_Impl = UserImpl

  val make_user : string -> int -> User_Impl.t
  val deposit : User_Impl.t -> int -> User_Impl.t
  val withdraw : User_Impl.t -> int -> User_Impl.t
  val buy : User_Impl.t -> string -> int -> User_Impl.t
  val sell : User_Impl.t -> string -> int -> User_Impl.t
  val next_day : User_Impl.t -> User_Impl.t

  (* val get_stock : string -> int *)
  val view_portfolio : User_Impl.t -> (string * int) list
  val view_balance : User_Impl.t -> int
end

module Cli : CliType
