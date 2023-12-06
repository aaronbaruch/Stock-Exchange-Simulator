(* Test plan for Stocks: The test plan for Stocks is a system of combination of
   manually test and OUnit test. Furthermore, the test suite focuses and tests
   the modules Cli, Data, User. The test cases were developed by both glassbox
   and blackbox testing to ensure implementation, typical cases, edge cases, and
   specification is correct. Hence, we have a very inclusive test planning to
   ensure our program's correctness. Because, we have our modules covered by
   both glassbox and blackbox testing ensuring their functionality, and it's all
   tested together with the interactable system using manual testing and
   ensuring our program comes together as we originally planned and works out as
   intended. TALK ABOUT HOW WE CANT TEST EVERYTHING BECuase SOME HAVE WIERD
   OUTPUTS LIKE NEWS, as we try to get as CLOSE TO MAX Coverage *)

open Stocks
open OUnit2

let empty_user = Cli.Cli.make_user "empty_username" 0.00 true
let nonempty_user = Cli.Cli.make_user "nonempty_username" 1000.00 true
let wealthy_user = Cli.Cli.make_user "nonempty_username" 1000000.00 true

(* let one_user = Cli.Cli.buy (Cli.Cli.make_user "nonempty_username" 1000000.00
   true) "AAPL" 5

   let two_user = Cli.Cli.buy (Cli.Cli.buy (Cli.Cli.make_user
   "nonempty_username" 1000000.00 true) "AAPL" 5) "IBM" 5 *)

let float_test out in1 _ =
  assert_equal ~msg:"Float Test" ~printer:(fun x -> string_of_float x) out in1

let string_of_portfolio pairs =
  let concat_pair acc (str, num) = acc ^ str ^ string_of_int num in
  List.fold_left concat_pair "" pairs

let portfolio_test out in1 _ =
  assert_equal ~msg:"Portfolio Test"
    ~printer:(fun x -> string_of_portfolio x)
    out in1

let user_cli_suite =
  [
    "Empty User balance" >:: float_test 0.00 (Cli.Cli.view_balance empty_user);
    "Non-Empty User balance"
    >:: float_test 1000.00 (Cli.Cli.view_balance nonempty_user);
    "Deposit into empty user"
    >:: float_test 50.00
          (Cli.Cli.view_balance (Cli.Cli.deposit empty_user 50.00));
    "Deposit into non-empty user"
    >:: float_test 1500.00
          (Cli.Cli.view_balance (Cli.Cli.deposit nonempty_user 500.00));
    "Withdraw from non-empty"
    >:: float_test 100.00
          (Cli.Cli.view_balance (Cli.Cli.withdraw nonempty_user 900.00));
    "Witdraw from empty"
    >:: float_test 0.00
          (Cli.Cli.view_balance
             (Cli.Cli.withdraw
                (Cli.Cli.make_user "empty_username" 0.00 true)
                5000.00));
    "Overdraw"
    >:: float_test 1000.00
          (Cli.Cli.view_balance (Cli.Cli.withdraw nonempty_user 1500.00));
    "Buying typical stock"
    >:: portfolio_test
          [ ("AAPL", 1) ]
          (Cli.Cli.view_portfolio (Cli.Cli.buy wealthy_user "AAPL" 1));
    (* "Buying zero stock" >:: portfolio_test [ ("AAPL", 1) ]
       (Cli.Cli.view_portfolio (Cli.Cli.buy wealthy_user "AAPL" 1)); *)
    (* Not used, tested manually *)
    "Buying 1000 of stock"
    >:: portfolio_test
          [ ("AAPL", 1000) ]
          (Cli.Cli.view_portfolio (Cli.Cli.buy wealthy_user "AAPL" 1000));
    "Buying 2\n   100 shares of stock"
    >:: portfolio_test
          [ ("IBM", 100); ("AAPL", 100) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.buy (Cli.Cli.buy wealthy_user "AAPL" 100) "IBM" 100));
    "Buying 2 10 and 1 5 shares of stock"
    >:: portfolio_test
          [ ("SPY", 10); ("IBM", 5); ("AAPL", 10) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.buy
                (Cli.Cli.buy (Cli.Cli.buy wealthy_user "AAPL" 10) "IBM" 5)
                "SPY" 10));
    "Buying 1 stock, not affordable"
    >:: portfolio_test []
          (Cli.Cli.view_portfolio (Cli.Cli.buy empty_user "AAPL" 1));
    "Buying 100\n   stock, not affordable"
    >:: portfolio_test []
          (Cli.Cli.view_portfolio (Cli.Cli.buy empty_user "AAPL" 100));
    "Buying 100 stock, not affordable"
    >:: portfolio_test []
          (Cli.Cli.view_portfolio (Cli.Cli.buy empty_user "AAPL" 100));
    "Buying 100 stock, not affordable"
    >:: portfolio_test []
          (Cli.Cli.view_portfolio (Cli.Cli.buy empty_user "AAPL" 100));
    (* What about not max int *)
  ]

let test_suite =
  "Trading functionality test suite " >::: List.flatten [ user_cli_suite ]

let () = run_test_tt_main test_suite
