open Stocks
open OUnit2

let empty_user = Cli.Cli.make_user "empty_username" 0.00
let nonempty_user = Cli.Cli.make_user "nonempty_username" 1000.00

let int_test out in1 _ =
  assert_equal ~msg:"Integer Test" ~printer:(fun x -> string_of_float x) out in1

let user_cli_suite =
  [
    "Empty User balance" >:: int_test 0.00 (Cli.Cli.view_balance empty_user);
    "Non-Empty User balance"
    >:: int_test 1000.00 (Cli.Cli.view_balance nonempty_user);
    "Deposit into empty user"
    >:: int_test 50.00 (Cli.Cli.view_balance (Cli.Cli.deposit empty_user 50.00));
    "Deposit into non-empty user"
    >:: int_test 1500.00
          (Cli.Cli.view_balance (Cli.Cli.deposit nonempty_user 500.00));
    "Withdraw from non-empty"
    >:: int_test 100.00
          (Cli.Cli.view_balance (Cli.Cli.withdraw nonempty_user 900.00));
    "Witdraw from empty"
    >:: int_test 0.00
          (Cli.Cli.view_balance
             (Cli.Cli.withdraw
                (Cli.Cli.make_user "empty_username" 0.00)
                5000.00));
    "Overdraw"
    >:: int_test 1000.00
          (Cli.Cli.view_balance (Cli.Cli.withdraw nonempty_user 1500.00));
    (* What about not max int *)
  ]

let test_suite =
  "Trading functionality test suite " >::: List.flatten [ user_cli_suite ]

let () = run_test_tt_main test_suite
