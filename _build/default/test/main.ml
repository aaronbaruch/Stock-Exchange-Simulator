open Stocks
open OUnit2

let user = ref (Cli.Cli.make_user "username" 0)

let this_suite =
  [ ("" >:: fun _ -> assert_equal 0 (Cli.Cli.view_balance !user)) ]

let test_suite = "Test" >::: List.flatten [ this_suite ]
let () = run_test_tt_main test_suite
