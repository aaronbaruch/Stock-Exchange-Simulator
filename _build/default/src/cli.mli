(** Module providing functions for user interactions and financial data
    retrieval. *)

open User
open Data

module type CliType = sig
  module User_Impl = UserImpl
  (** Module type representing the User module, providing functions for user
      interactions with a financial system. *)

  module Data_Impl = DataAPI
  (** Module type representing the Data module, providing functions for
      retrieving financial data and information. *)

  type stock_query = string * int

  val make_user : string -> float -> bool -> User_Impl.t
  (** [make_user username balance] creates a new user account with the given
      [username] and initial [balance]. This user starts with an empty portfolio
      and day 0. *)

  val deposit : User_Impl.t -> float -> User_Impl.t
  (** [deposit user n] increases the user's balance by [n] dollars. *)

  val withdraw : User_Impl.t -> float -> User_Impl.t
  (** [withdraw user n] decreases the user's balance by [n] dollars. *)

  val buy : User_Impl.t -> string -> int -> User_Impl.t
  (** [buy user index n] user [user] buys [n] shares of a stock of index
      [index]. It deducts the cost from the user's balance and updates their
      stock portfolio. Returns either the original [user] as the input if the
      stock could not be bought. Returns the edited user with bought stock if
      the stock could be bought. *)

  val sell : User_Impl.t -> string -> int -> User_Impl.t
  (** [sell user index n] user [user] sells [n] shares of a stock of index
      [index]. It adds to the user's balance and updates their stock portfolio.
      Returns either the original [user] as the input if the stock could not be
      sold (trying to sell stock they don't have). Returns the edited user with
      sold stock if the stock could be sold. *)

  val next_day : User_Impl.t -> User_Impl.t
  (** [next_day user] iterates the user by 1 to the next day. *)

  val view_portfolio : User_Impl.t -> (string * int) list
  (** [view_portfolio user] returns an (string, int) list of the user's
      portfolio. *)

  val view_balance : User_Impl.t -> float
  (** [view_balance user] returns a float of the user's balance. *)

  val view_ledger : User_Impl.t -> User_Impl.ledger_entry list ref
  (** [view_ledger user] returns a reference to the ledger of the user. *)

  val calculate_stock_correlation :
    User_Impl.t -> string -> string -> int -> float
  (** [calculate_stock_correlation symbol1 symbol2 days] calculates the
      correlation coefficient between two stocks represented by [symbol1] and
      [symbol2] over the specified number of [days]. *)

  val get_latest_news_feeds : User_Impl.t -> string -> string
  (** [get_latest_news_feeds symbol] returns a formatted string containing the
      latest news feeds for the specified stock symbol. *)

  val generate_stock_summary : User_Impl.t -> string -> string
  (** [generate_stock_summary symbol] generates a summary for the specified
      stock symbol, including relevant information such as price, volume, and
      news sentiment. *)
end

module Cli : CliType
(** Implementation of CliType *)
