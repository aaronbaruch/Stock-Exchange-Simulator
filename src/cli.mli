module type CliType = sig
  type t
  
  val make_user : string -> int -> User.t

  val deposit : int -> bool

  val withdraw : int -> bool
  
  val buy : int -> bool

  val sell : int -> bool

  val next_day : Data.t

  val get_stock : string -> int

  val view_portfolio : User.t
end