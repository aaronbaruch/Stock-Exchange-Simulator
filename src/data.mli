module type Data = sig
  val get_date : unit -> string

  val get_ticker_price : string -> string Lwt.t
  (** Given the TICKER of a stock, returns its price. Returns -1 if invalid
      ticker or other error *)

  val calculate_stock_correlation : string -> string -> int -> float Lwt.t
  (** Given the ticker of two stocks, returns the correlation rate between them
      throughout the past X days*)

  val get_latest_news_feeds : string -> (string * string * float) list Lwt.t
  (** Given the ticker of a stock, returns a list of the 5 most recent news
      articles in a (title, summary, sentiment_score) list*)

  val generate_stock_summary : string -> string Lwt.t
  (** Given the ticker of a stock, returns a key analytics about the stock*)
end

module DataAPI : Data
