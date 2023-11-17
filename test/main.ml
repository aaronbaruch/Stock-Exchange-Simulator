(* open Stocks *)
open OUnit2

let this_suite = [ ("" >:: fun _ -> assert_equal true true) ]
let test_suite = "Test" >::: List.flatten [ this_suite ]
let () = run_test_tt_main test_suite
