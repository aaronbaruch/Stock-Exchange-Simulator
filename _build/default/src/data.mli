module type Data = sig
  val get_ticker_price : int -> string -> int
  (** Given the day and TICKER of a stock, returns its price. Returns -1 if
      invalid ticker or other error *)
end
