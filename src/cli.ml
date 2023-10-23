open User

module type CliType = sig
  module User_Impl = UserImpl

  val make_user : string -> int -> User_Impl.t
  val deposit : User_Impl.t -> int -> User_Impl.t
  val withdraw : User_Impl.t -> int -> User_Impl.t
  val buy : User_Impl.t -> string -> int -> User_Impl.t
  val sell : User_Impl.t -> string -> int -> User_Impl.t
  val next_day : User_Impl.t -> User_Impl.t

  (* val get_stock : User_Impl -> int -> string -> int *)
  val view_portfolio : User_Impl.t -> (string * int) list
end

module Cli : CliType = struct
  module User_Impl = UserImpl

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
