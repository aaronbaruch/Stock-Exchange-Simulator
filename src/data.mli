module type Data = sig
  val get_ticker_price : string -> string Lwt.t
  (** Given the day and TICKER of a stock, returns its price. Returns -1 if
      invalid ticker or other error *)
end

module DataAPI : Data
