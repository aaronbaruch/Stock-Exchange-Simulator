(** Module providing functions for retrieving financial data and information. *)

module type Data = sig
  val get_date : unit -> string
  (** [get_date ()] returns the current date as a formatted string. *)

  val get_ticker_price : string -> string Lwt.t
  (** [get_ticker_price symbol] returns a promise (Lwt.t) of the latest ticker
      price for the specified stock [symbol]. *)

  val calculate_stock_correlation : string -> string -> int -> float Lwt.t
  (** [calculate_stock_correlation symbol1 symbol2 days] returns a promise
      (Lwt.t) of the correlation coefficient between two stocks represented by
      [symbol1] and [symbol2] over the specified number of [days]. *)

  val get_latest_news_feeds : string -> (string * string * float) list Lwt.t
  (** [get_latest_news_feeds symbol] returns a promise (Lwt.t) of a list
      containing the latest news feeds for the specified stock [symbol]. Each
      news item is represented as a tuple (title, summary, sentiment_score). *)

  val generate_stock_summary : string -> string Lwt.t
  (** [generate_stock_summary symbol] returns a promise (Lwt.t) of a summary for
      the specified stock [symbol], including relevant information such as
      current price, price a year ago, high and low prices, and average trading
      volume. *)
end

module DataAPI : Data
(** Implementation of Data. *)
