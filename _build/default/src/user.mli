module type User = sig
  type action
  type ledger_entry
  type t

  val init_user : string -> float -> t
  val deposit : t -> float -> t
  val withdraw : t -> float -> t
  val balance : t -> float
  val portfolio : t -> (string * int) list
  val ledger : t -> ledger_entry list ref
  val sell : t -> string -> int -> t
  val buy : t -> string -> int -> t
  val next_day : t -> t
  val display_username : t -> string
  val print_ledger : ledger_entry list ref -> unit
end

module UserImpl : User
