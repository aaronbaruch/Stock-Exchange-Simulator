(** open Bag *)

(** A model can predict the next word. *)
module type Data = sig
  (** Representation type of the model. *)

  val get_ticker_price : int -> string -> int
  (** Given the day and TICKER of a stock, returns its price. Returns -1 if invalid ticker or other error *)
end