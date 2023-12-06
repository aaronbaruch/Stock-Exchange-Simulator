(** Module defines User and functions to interact with a financial system *)

module type User = sig
  type action
  (** Type representing an action that the user can perform: Buy, Sell, Deposit,
      or Withdraw *)

  type ledger_entry
  (** Type representing a ledger_entry. Constains fields for date, action, and
      balance *)

  type t
  (** Type representing user. Contains fields for user info: username, balance,
      stocks, day, ledger *)


  val init_user : string -> float -> bool -> t
  (** [init_user username balance] creates a new user account with the given
      [username] and initial [balance]. This user starts with an empty portfolio
      and day 0. *)
      
  val deposit : t -> float -> t
  (** [deposit user n] increases the user's balance by [n] dollars *)

  val withdraw : t -> float -> t
  (** [withdraw user n] decreases the user's balance by [n] dollars *)

  val balance : t -> float
  (** [balance user] returns a float of the user's balance *)

  val portfolio : t -> (string * int) list
  (** [balance user] returns an (string, int) list of the user's portfolio. *)

  val ledger : t -> ledger_entry list ref
  (** [ledger user] returns a list of ledger_entry *)

  val sell : t -> string -> int -> t
  (** [sell user index n] user [user] sells [n] shares of a stock of index
      [index]. It adds to the user's balance and updates their stock portfolio.
      Returns either the original [user] as the input if the stock could not be
      sold (trying to sell stock they don't have). Returns the edited user with
      sold stock if the stock could be sold.*)

  val buy : t -> string -> int -> t
  (** [buy user index n] user [user] buys [n] shares of a stock of index
      [index]. It deducts the cost from the user's balance and updates their
      stock portfolio. Returns either the original [user] as the input if the
      stock could not be bought. Returns the edited user with bought stock if
      the stock could be bought.*)

  val next_day : t -> t
  (** [next_day user] iterates the user by 1 to the next day*)

  val display_username : t -> string
  (** [display_username user] returns the string of the username *)


  val get_day : t -> int
  val print_ledger : ledger_entry list ref -> unit
  (** [print_ledger ledger] prints the given ledger *)
end

module UserImpl : User
(** Implementation of User *)
