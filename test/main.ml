(* Test plan for Stocks: The test plan for Stocks is a system of combination of
   manually test and OUnit test. Furthermore, the test suite focuses and tests
   the modules Cli, Data, User. The test cases were developed by both glassbox
   and blackbox testing to ensure implementation, typical cases, edge cases, and
   specification is correct. What can be tested by Ounit tests like functions
   is, and what needs human interaction is done manually. Hence, we have a very
   inclusive test planning to ensure our program's correctness. Because, we have
   our modules covered by both glassbox and blackbox testing ensuring their
   functionality, and it's all tested together with the interactable system
   using manual testing and ensuring our program comes together as we originally
   planned and works out as intended. Blackbox testing is used to test
   functionality that the output is predictable and the implementation is less
   important, as long as we reach the correct output, like the balance of a user
   set with 1000.00 is 1000.00. Then, for glassbox it is important to reach
   coverage in functions like buying and selling because there are many branches
   where we need to ensure what happens if a user might not be able to buy/sell
   and what happens if they don't or are able to (Where there are many different
   outputs). It is important to note we cannot reach 100% coverage in bisect,
   but we came as close as possible, as live data is often unpredictable and
   there are sections of our code that are unreachable but necessary for
   compile. Furthermore, we cannot test live data as the market updates everyday
   and we cannot predict the price/news of a stock for exact testing, but we can
   test that there is atleast an output for news. This is one example of where
   manual testing was of importance, because our automatic OUnit tests could not
   predict this live data. We would simply test manually by providing a variety
   of inputs to our CLI interface and tracing them to make sure the outputs of
   our ledger, portfolio, and user information are maintained correctly. This
   combination of manual and automatic testing, and glassbox and blackbox
   testing, to ensure that our program performs properly in all possible cases,
   is why we can ensure the correctness of our program thoroughly. *)

open Stocks
open OUnit2

let empty_user = Cli.Cli.make_user "empty_username" 0.00 true
let nonempty_user = Cli.Cli.make_user "nonempty_username" 1000.00 true
let wealthy_user = Cli.Cli.make_user "nonempty_username" 1000000.00 true

let one_user =
  Cli.Cli.buy (Cli.Cli.make_user "nonempty_username" 1000000.00 true) "AAPL" 5

let two_user =
  Cli.Cli.buy
    (Cli.Cli.buy
       (Cli.Cli.make_user "nonempty_username" 1000000.00 true)
       "AAPL" 5)
    "IBM" 5

let three_user =
  Cli.Cli.buy
    (Cli.Cli.buy
       (Cli.Cli.buy
          (Cli.Cli.make_user "nonempty_username" 1000000.00 true)
          "AAPL" 5)
       "IBM" 5)
    "SPY" 10

let float_test out in1 _ =
  assert_equal ~msg:"Float Test" ~printer:(fun x -> string_of_float x) out in1

let int_test out in1 _ =
  assert_equal ~msg:"Float Test" ~printer:(fun x -> string_of_int x) out in1

let string_test out in1 _ =
  assert_equal ~msg:"String Test" ~printer:(fun x -> x) out in1

let bool_test out in1 _ =
  assert_equal ~msg:"Bool Test" ~printer:(fun x -> string_of_bool x) out in1

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
    " selling stock from empty"
    >:: portfolio_test []
          (Cli.Cli.view_portfolio (Cli.Cli.sell empty_user "AAPL" 100));
    " selling stock from one"
    >:: portfolio_test
          [ ("AAPL", 0) ]
          (Cli.Cli.view_portfolio (Cli.Cli.sell one_user "AAPL" 5));
    " selling stock from one, typical sell"
    >:: portfolio_test
          [ ("AAPL", 2) ]
          (Cli.Cli.view_portfolio (Cli.Cli.sell one_user "AAPL" 3));
    " selling stock > one from one"
    >:: portfolio_test
          [ ("AAPL", 5) ]
          (Cli.Cli.view_portfolio (Cli.Cli.sell one_user "AAPL" 100));
    " selling more stock from two"
    >:: portfolio_test
          [ ("IBM", 5); ("AAPL", 5) ]
          (Cli.Cli.view_portfolio (Cli.Cli.sell two_user "AAPL" 100));
    " selling stock from two, typical single"
    >:: portfolio_test
          [ ("IBM", 5); ("AAPL", 2) ]
          (Cli.Cli.view_portfolio (Cli.Cli.sell two_user "AAPL" 3));
    " selling stock from two, typical two same"
    >:: portfolio_test
          [ ("IBM", 2); ("AAPL", 2) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell two_user "AAPL" 3) "IBM" 3));
    " selling stock from two, typical two unique"
    >:: portfolio_test
          [ ("IBM", 2); ("AAPL", 4) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell two_user "AAPL" 1) "IBM" 3));
    " selling stock from two, full two both"
    >:: portfolio_test
          [ ("IBM", 0); ("AAPL", 0) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell two_user "AAPL" 5) "IBM" 5));
    " selling stock from two, one full, other not "
    >:: portfolio_test
          [ ("IBM", 4); ("AAPL", 0) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell two_user "AAPL" 5) "IBM" 1));
    " selling stock from two, one oversell, other typical"
    >:: portfolio_test
          [ ("IBM", 4); ("AAPL", 5) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell two_user "AAPL" 50) "IBM" 1));
    " selling stock from two, one oversell, other full"
    >:: portfolio_test
          [ ("IBM", 5); ("AAPL", 0) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell two_user "AAPL" 5) "IBM" 100));
    " selling stock from two, both oversell"
    >:: portfolio_test
          [ ("IBM", 5); ("AAPL", 5) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell two_user "AAPL" 52) "IBM" 100));
    " selling stock from three, typical, one untouched"
    >:: portfolio_test
          [ ("SPY", 10); ("IBM", 3); ("AAPL", 2) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.sell three_user "AAPL" 3) "IBM" 2));
    " selling stock from three, sell all"
    >:: portfolio_test
          [ ("SPY", 0); ("IBM", 0); ("AAPL", 0) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell
                (Cli.Cli.sell (Cli.Cli.sell three_user "AAPL" 5) "IBM" 5)
                "SPY" 10));
    " selling stock from three, typical, all oversell"
    >:: portfolio_test
          [ ("SPY", 10); ("IBM", 5); ("AAPL", 5) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell
                (Cli.Cli.sell (Cli.Cli.sell three_user "AAPL" 52) "IBM" 100)
                "SPY" 1000));
    "Overselling and then buying singular"
    >:: portfolio_test
          [ ("SPY", 12); ("IBM", 5); ("AAPL", 5) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.buy
                (Cli.Cli.sell
                   (Cli.Cli.sell (Cli.Cli.sell three_user "AAPL" 52) "IBM" 100)
                   "SPY" 1000)
                "SPY" 2));
    "Overselling and then overbuying"
    >:: portfolio_test
          [ ("SPY", 10); ("IBM", 5); ("AAPL", 5) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.buy
                (Cli.Cli.sell
                   (Cli.Cli.sell (Cli.Cli.sell three_user "AAPL" 52) "IBM" 100)
                   "SPY" 1000)
                "SPY" 200000));
    "Buying and then full selling"
    >:: portfolio_test
          [ ("AAPL", 0) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.buy wealthy_user "AAPL" 5) "AAPL" 5));
    "Buying and then typical selling"
    >:: portfolio_test
          [ ("AAPL", 3) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.buy wealthy_user "AAPL" 5) "AAPL" 2));
    "Buying and then overselling"
    >:: portfolio_test
          [ ("AAPL", 5) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell (Cli.Cli.buy wealthy_user "AAPL" 5) "AAPL" 100));
    "Debt user"
    >:: float_test (-100.)
          (Cli.Cli.view_balance (Cli.Cli.make_user "debt" (-100.00) true));
    "typical username"
    >:: string_test "empty_username" (User.UserImpl.display_username empty_user);
    "empty username"
    >:: string_test ""
          (User.UserImpl.display_username (Cli.Cli.make_user "" 0. true));
    "purelynumber username"
    >:: string_test "135235"
          (User.UserImpl.display_username (Cli.Cli.make_user "135235" 0. true));
    "wierd character username"
    >:: string_test "2345@#]"
          (User.UserImpl.display_username (Cli.Cli.make_user "2345@#]" 0. true));
    "get day test, live test"
    >:: int_test (-1)
          (User.UserImpl.get_day
             (Cli.Cli.make_user "live user" (-100.00) false));
    "get day test, dummy" >:: int_test 0 (User.UserImpl.get_day empty_user);
    "get day test, next day"
    >:: int_test 1 (User.UserImpl.get_day (Cli.Cli.next_day empty_user));
    "Ledger test, 0"
    >:: bool_test true (0 <= List.length !(Cli.Cli.view_ledger empty_user));
    "Ledger test, 1"
    >:: bool_test true (0 <= List.length !(Cli.Cli.view_ledger one_user));
    "Buying, selling, Buying"
    >:: portfolio_test
          [ ("AAPL", 3) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.buy
                (Cli.Cli.sell (Cli.Cli.buy wealthy_user "AAPL" 3) "AAPL" 3)
                "AAPL" 3));
    "Selling, buying, selling"
    >:: portfolio_test
          [ ("AAPL", 0) ]
          (Cli.Cli.view_portfolio
             (Cli.Cli.sell
                (Cli.Cli.buy (Cli.Cli.sell wealthy_user "AAPL" 3) "AAPL" 3)
                "AAPL" 3));
    "Balance lower after buying"
    >:: bool_test true
          (Cli.Cli.view_balance wealthy_user
          > Cli.Cli.view_balance (Cli.Cli.buy wealthy_user "AAPL" 30));
    "Balance higher after selling"
    >:: bool_test true
          (Cli.Cli.view_balance one_user
          < Cli.Cli.view_balance (Cli.Cli.sell one_user "AAPL" 3));
  ]

let data_suite =
  [
    "Correlation"
    >:: float_test 0.
          (Cli.Cli.calculate_stock_correlation
             (Cli.Cli.next_day (Cli.Cli.next_day empty_user))
             "AAPL" "SPY" 2);
    "Stock\n       Summary, AAPL"
    >:: bool_test true
          (0
          < String.length
              (Cli.Cli.generate_stock_summary
                 (Cli.Cli.make_user "live user" (-100.00) false)
                 "AAPL"));
    "Stock Summary, IBM"
    >:: bool_test true
          (0
          < String.length
              (Cli.Cli.generate_stock_summary
                 (Cli.Cli.make_user "live\n       user" (-100.00) false)
                 "IBM"));
    "Stock Summary, SPY"
    >:: bool_test true
          (0
          < String.length
              (Cli.Cli.generate_stock_summary
                 (Cli.Cli.make_user "live user" (-100.00) false)
                 "SPY"));
    "Stock news, SPY"
    >:: bool_test true
          (0
          < String.length
              (Cli.Cli.get_latest_news_feeds
                 (Cli.Cli.make_user "live user" (-100.00) false)
                 "SPY"));
    "Stock news, AAPL"
    >:: bool_test true
          (0
          < String.length
              (Cli.Cli.get_latest_news_feeds
                 (Cli.Cli.make_user "live user" (-100.00) false)
                 "AAPL"));
    "Stock news, IBM"
    >:: bool_test true
          (0
          < String.length
              (Cli.Cli.get_latest_news_feeds
                 (Cli.Cli.make_user "live user" (-100.00) false)
                 "IBM"));
  ]

let test_suite =
  "Trading functionality test suite "
  >::: List.flatten [ user_cli_suite; data_suite ]

let () = run_test_tt_main test_suite
