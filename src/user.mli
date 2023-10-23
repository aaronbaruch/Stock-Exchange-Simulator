module type User = sig
  type t

  val init_user : string -> int -> t
  val deposit : t -> int -> t
  val withdraw : t -> int -> t
  val balance : t -> int
  val portfolio : t -> (string * int) list
  val sell : t -> string -> int -> t
  val buy : t -> string -> int -> t
  val next_day : t -> t
  val display_username : t -> string
end

module UserImpl : User
