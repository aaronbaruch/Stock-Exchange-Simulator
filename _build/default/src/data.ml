open Cohttp_lwt_unix
open Lwt

module type Data = sig
  val get_ticker_price : string -> string Lwt.t
  val calculate_stock_correlation : string -> string -> int -> float Lwt.t
end

module DataAPI : Data = struct
  let base_url = "https://www.alphavantage.co/query"
  let api_key = "RYGDU4CNOH0N3NQU"

  let get_ticker_price symbol =
    let uri = Uri.of_string base_url in
    let params =
      [
        ("function", [ "TIME_SERIES_DAILY" ]);
        ("symbol", [ symbol ]);
        ("apikey", [ api_key ]);
      ]
    in
    let uri = Uri.add_query_params uri params in
    Client.get uri >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >>= fun body ->
    match resp.status with
    | `OK -> (
        try
          let json = Yojson.Basic.from_string body in
          let get_most_recent_close json =
            match json with
            | `Assoc root_list -> (
                match List.assoc_opt "Time Series (Daily)" root_list with
                | Some (`Assoc time_series_list) -> (
                    let most_recent_date, _ = List.hd time_series_list in
                    match List.assoc_opt most_recent_date time_series_list with
                    | Some (`Assoc prices) -> (
                        match List.assoc_opt "4. close" prices with
                        | Some (`String close_price) -> Lwt.return close_price
                        | _ -> Lwt.fail_with "No close price found")
                    | _ -> Lwt.fail_with "No time series data found")
                | _ -> Lwt.fail_with "No 'Time Series (Daily)' key found")
            | _ -> Lwt.fail_with "Expected JSON object"
          in
          get_most_recent_close json
        with
        | Yojson.Json_error msg -> Lwt.fail_with msg
        | _ -> Lwt.fail_with "Unexpected error while parsing JSON")
    | _ -> Lwt.fail_with "HTTP request failed"

  let mean lst =
    let sum = List.fold_left ( +. ) 0.0 lst in
    sum /. float_of_int (List.length lst)

  let variance lst =
    let m = mean lst in
    List.fold_left (fun acc x -> acc +. ((x -. m) ** 2.)) 0.0 lst
    /. float_of_int (List.length lst)

  let standard_deviation lst = sqrt (variance lst)

  let covariance lst1 lst2 =
    let m1 = mean lst1 in
    let m2 = mean lst2 in
    let pairs = List.combine lst1 lst2 in
    List.fold_left (fun acc (x, y) -> acc +. ((x -. m1) *. (y -. m2))) 0.0 pairs
    /. float_of_int (List.length pairs)

  let pearson_correlation lst1 lst2 =
    let cov = covariance lst1 lst2 in
    let std_dev1 = standard_deviation lst1 in
    let std_dev2 = standard_deviation lst2 in
    cov /. (std_dev1 *. std_dev2)

  let get_historical_prices symbol days =
    let uri = Uri.of_string base_url in
    let params =
      [
        ("function", [ "TIME_SERIES_DAILY" ]);
        ("outputsize", [ "full" ]);
        ("symbol", [ symbol ]);
        ("apikey", [ api_key ]);
      ]
    in
    let uri = Uri.add_query_params uri params in
    Client.get uri >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >>= fun body ->
    match resp.status with
    | `OK -> (
        try
          let json = Yojson.Basic.from_string body in
          let get_closing_prices json =
            match json with
            | `Assoc root_list -> (
                match List.assoc_opt "Time Series (Daily)" root_list with
                | Some (`Assoc time_series_list) ->
                    let sorted_dates =
                      List.sort
                        (fun (d1, _) (d2, _) -> compare d2 d1)
                        time_series_list
                    in
                    let rec extract_closing_prices dates acc count =
                      match dates with
                      | [] -> Lwt.return (List.rev acc)
                      | (_, `Assoc prices) :: tail when count > 0 -> (
                          match List.assoc_opt "4. close" prices with
                          | Some (`String close_price) ->
                              extract_closing_prices tail (close_price :: acc)
                                (count - 1)
                          | _ -> Lwt.fail_with "No close price found")
                      | _ -> Lwt.return (List.rev acc)
                    in
                    extract_closing_prices sorted_dates [] days
                | _ -> Lwt.fail_with "No 'Time Series (Daily)' key found")
            | _ -> Lwt.fail_with "Expected JSON object"
          in
          get_closing_prices json
        with
        | Yojson.Json_error msg -> Lwt.fail_with msg
        | _ -> Lwt.fail_with "Unexpected error while parsing JSON")
    | _ -> Lwt.fail_with "HTTP request failed"

  let get_historical_prices_as_float symbol days =
    let%lwt prices = get_historical_prices symbol days in
    Lwt.return (List.map float_of_string prices)

  let take n lst =
    let rec aux n acc lst =
      if n <= 0 then List.rev acc
      else
        match lst with
        | [] -> List.rev acc
        | x :: xs -> aux (n - 1) (x :: acc) xs
    in
    aux n [] lst

  let calculate_stock_correlation symbol1 symbol2 days =
    let%lwt prices1 = get_historical_prices_as_float symbol1 days in
    let%lwt prices2 = get_historical_prices_as_float symbol2 days in
    (* Ensure that the lists have the same length *)
    let aligned_prices1, aligned_prices2 =
      let len = min (List.length prices1) (List.length prices2) in
      (take len prices1, take len prices2)
    in
    Lwt.return (pearson_correlation aligned_prices1 aligned_prices2)
end
