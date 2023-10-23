open Yojson.Basic.Util

module type Data = sig
  val get_ticker_price : int -> string -> int
  (** Given the day and TICKER of a stock, returns its price. Returns -1 if
      invalid ticker or other error *)
end

module DataStorage : Data = struct
  (* Load JSON data from file *)
  let prices = try Yojson.Basic.from_file "prices.json" with _ -> `Null

  let get_ticker_price day ticker =
    try
      prices
      |> member (string_of_int day)
      |> member ticker |> to_string |> int_of_string
    with _ -> -1
end
