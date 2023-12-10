(** Module providing functions for retrieving financial data and information. *)

module type Data = sig
  type stock_query = string * int
  (** The stock_query is designed to retrieve historical stock data and is
      structured in the format (ticker, days_back). Here, ticker represents the
      stock symbol of the company you are interested in. The days_back parameter
      specifies the number of days from the current date for which you want to
      retrieve data. For example, if days_back is set to 10, the query will
      fetch stock data from 10 days ago up to the present day. *)

  val get_date : int -> string Lwt.t
  (** [get_date n] returns the date of the current day as a formatted string. *)

  val get_ticker_price : stock_query -> string Lwt.t
  (** [get_ticker_price symbol] returns a promise (Lwt.t) of the latest ticker
      price for the specified stock [symbol]. *)

  val calculate_stock_correlation :
    stock_query -> stock_query -> int -> float Lwt.t
  (** [calculate_stock_correlation symbol1 symbol2 days] returns a promise
      (Lwt.t) of the correlation coefficient between two stocks represented by
      [symbol1] and [symbol2] over the specified number of [days]. *)

  val get_latest_news_feeds :
    stock_query -> (string * string * float) list Lwt.t
  (** [get_latest_news_feeds symbol] returns a promise (Lwt.t) of a list
      containing the latest news feeds for the specified stock [symbol]. Each
      news item is represented as a tuple (title, summary, sentiment_score). *)

  val generate_stock_summary : stock_query -> string Lwt.t
  (** [generate_stock_summary symbol] returns a promise (Lwt.t) of a summary for
      the specified stock [symbol], including relevant information such as
      current price, price a year ago, high and low prices, and average trading
      volume. *)
end

module DataAPI : Data
(** Implementation of Data. *)
