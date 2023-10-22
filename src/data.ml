(*opam install yojson*)
open Yojson.Basic.Util

module type Data = sig
  val get_ticker_price : int -> string -> int
  (** Given the day and TICKER of a stock, returns its price. Returns -1 if invalid ticker or other error *)
end

module DataStorage : Data = struct
  (* Load JSON data from file *)
  let prices = try Yojson.Basic.from_file "prices.json" |> to_list with _ -> []

  (* Find price given a day and ticker *)
  let rec find_price day ticker = function
    | [] -> -1
    | h::t ->
      match (h |> member "day" |> to_int, h |> member "ticker" |> to_string) with
      | (d, tkr) when d = day && tkr = ticker -> h |> member "price" |> to_int
      | _ -> find_price day ticker t

  let get_ticker_price day ticker = find_price day ticker prices
end
