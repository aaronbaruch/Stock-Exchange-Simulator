module type Data = sig
  type stock_query = string * int

  val get_date : unit -> string

  val get_ticker_price : stock_query -> string Lwt.t
  (** Given the TICKER of a stock and days back, returns its price. Returns -1
      if invalid ticker or other error *)

  val calculate_stock_correlation :
    stock_query -> stock_query -> int -> float Lwt.t
  (** Given the ticker of two stocks and days back, returns the correlation rate
      between them throughout the past X days*)

  val get_latest_news_feeds :
    stock_query -> (string * string * float) list Lwt.t
  (** Given the ticker of a stock and days back, returns a list of the 5 most
      recent news articles in a (title, summary, sentiment_score) list*)

  val generate_stock_summary : stock_query -> string Lwt.t
  (** Given the ticker of a stock and days back, returns key analytics about the
      stock*)
end

module DataAPI : Data
