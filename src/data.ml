open Cohttp_lwt_unix
open Lwt

module type Data = sig
  val get_ticker_price : string -> string Lwt.t
  val calculate_stock_correlation : string -> string -> int -> float Lwt.t
  val get_latest_news_feeds : string -> (string * string * float) list Lwt.t
  val generate_stock_summary : string -> string Lwt.t
end

module DataAPI : Data = struct
  let base_url = "https://www.alphavantage.co/query"
  let api_key = "GRVEB2QNCNZAX221"

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
                | _ ->
                    Lwt.fail_with "No 'Time Series (Daily)' key found"
                    (* This might need to be changed, to not stop loop after
                       invalid index TODO: PERHAPS MAKE FUNCTION HERE TO CHECK
                       IF AN INPUT IS A VALID TICKER *))
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

  let get_historical_volumes symbol days =
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
          let get_closing_vol json =
            match json with
            | `Assoc root_list -> (
                match List.assoc_opt "Time Series (Daily)" root_list with
                | Some (`Assoc time_series_list) ->
                    let sorted_dates =
                      List.sort
                        (fun (d1, _) (d2, _) -> compare d2 d1)
                        time_series_list
                    in
                    let rec extract_closing_vol dates acc count =
                      match dates with
                      | [] -> Lwt.return (List.rev acc)
                      | (_, `Assoc vol) :: tail when count > 0 -> (
                          match List.assoc_opt "5. volume" vol with
                          | Some (`String close_vol) ->
                              extract_closing_vol tail (close_vol :: acc)
                                (count - 1)
                          | _ -> Lwt.fail_with "No volume found")
                      | _ -> Lwt.return (List.rev acc)
                    in
                    extract_closing_vol sorted_dates [] days
                | _ -> Lwt.fail_with "No 'Time Series (Daily)' key found")
            | _ -> Lwt.fail_with "Expected JSON object"
          in
          get_closing_vol json
        with
        | Yojson.Json_error msg -> Lwt.fail_with msg
        | _ -> Lwt.fail_with "Unexpected error while parsing JSON")
    | _ -> Lwt.fail_with "HTTP request failed"

  let get_historical_volumes_as_float symbol days =
    let%lwt vols = get_historical_volumes symbol days in
    Lwt.return (List.map float_of_string vols)

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

  let rec take n lst =
    if n <= 0 then []
    else
      match lst with
      | [] -> []
      | x :: xs -> x :: take (n - 1) xs

  let get_latest_news_feeds symbol =
    let uri =
      Uri.of_string
        (base_url ^ "?function=NEWS_SENTIMENT&tickers=" ^ symbol ^ "&apikey="
       ^ api_key)
    in
    Client.get uri >>= fun (resp, body) ->
    body |> Cohttp_lwt.Body.to_string >>= fun body ->
    match resp.status with
    | `OK -> (
        try
          let json = Yojson.Basic.from_string body in
          let extract_news_data json =
            match json with
            | `Assoc json_assoc -> (
                match List.assoc_opt "feed" json_assoc with
                | Some (`List articles) ->
                    let first_five_articles = take 5 articles in
                    List.map
                      (fun article ->
                        match article with
                        | `Assoc article_data ->
                            let title =
                              match List.assoc_opt "title" article_data with
                              | Some (`String t) -> t
                              | _ -> "No title"
                            in
                            let summary =
                              match List.assoc_opt "summary" article_data with
                              | Some (`String s) -> s
                              | _ -> "No summary"
                            in
                            let sentiment_score =
                              match
                                List.assoc_opt "overall_sentiment_score"
                                  article_data
                              with
                              | Some (`Float s) -> s
                              | _ -> 0.0
                            in
                            (title, summary, sentiment_score)
                        | _ -> ("Invalid article format", "", 0.0))
                      first_five_articles
                | _ -> failwith "No 'feed' key found")
            | _ -> failwith "Unexpected JSON structure"
          in
          Lwt.return (extract_news_data json)
        with
        | Yojson.Json_error msg -> Lwt.fail_with msg
        | Failure msg -> Lwt.fail_with msg)
    | _ -> Lwt.fail_with "HTTP request failed"

  let generate_stock_summary ticker =
    let%lwt historical_prices = get_historical_prices_as_float ticker 365 in
    let%lwt historical_volumes = get_historical_volumes_as_float ticker 365 in
    let current_price = List.hd (List.rev historical_prices) in
    let year_ago_price = List.hd historical_prices in
    let high_price = List.fold_left max Float.min_float historical_prices in
    let low_price = List.fold_left min Float.max_float historical_prices in
    let average_volume = mean historical_volumes in
    let descriptor =
      if current_price > year_ago_price then "positive" else "negative"
    in
    Lwt.return
      (Printf.sprintf
         "The current price of %s is %.2f. Comparatively, a year ago, it was \
          trading at %.2f. This year has been %s for %s, reaching its peak \
          with a high of %.2f. Notably, the lowest point was with a price of \
          %.2f. The average trading volume this year was %.2f."
         ticker current_price year_ago_price descriptor ticker high_price
         low_price average_volume)
end
