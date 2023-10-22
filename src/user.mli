module type User = sig
  type t

  val init_user : int -> string
  val deposit : t -> int -> t
  val withdraw : t -> int -> t
  val balance : t -> int
  val portfolio : t -> (string * int) list
  val sell : t -> int -> t
  val buy : t -> string -> int -> t
end
