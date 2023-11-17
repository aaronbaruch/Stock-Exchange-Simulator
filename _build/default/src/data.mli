module type Data = sig
  val get_ticker_price : string -> string Lwt.t
  (** Given the TICKER of a stock, returns its price. Returns -1 if invalid
      ticker or other error *)

  val calculate_stock_correlation : string -> string -> int -> float Lwt.t
  (** Given the ticker of two stocks, returns the correlation rate between them
      throughout the past X days*)
end

module DataAPI : Data
