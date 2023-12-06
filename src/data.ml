open Cohttp_lwt_unix
open Lwt

module type Data = sig
  type stock_query = string * int

  val get_date : int -> string Lwt.t
  val get_ticker_price : stock_query -> string Lwt.t

  val calculate_stock_correlation :
    stock_query -> stock_query -> int -> float Lwt.t

  val get_latest_news_feeds :
    stock_query -> (string * string * float) list Lwt.t

  val generate_stock_summary : stock_query -> string Lwt.t
end

module DataAPI : Data = struct
  type stock_query = string * int

  let base_url = "https://www.alphavantage.co/query"
  let api_key = "60A00BFIIHK17O27"

  let get_date days_back =
    if days_back = -1 then
      (* Fetch the most recent date from the API *)
      let symbol = "aapl" in
      let uri = Uri.of_string base_url in
      let params =
        [
          ("function", [ "TIME_SERIES_DAILY" ]);
          ("symbol", [ symbol ]);
          ("apikey", [ api_key ]);
        ]
      in
      let uri = Uri.add_query_params uri params in
      let%lwt _, body = Client.get uri in
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      let json = Yojson.Basic.from_string body in
      match json with
      | `Assoc root_list -> (
          match List.assoc_opt "Time Series (Daily)" root_list with
          | Some (`Assoc time_series_list) ->
              let most_recent_date, _ = List.hd time_series_list in
              most_recent_date
          | _ -> failwith "No 'Time Series (Daily)' key found")
      | _ -> failwith "Expected JSON object"
    else
      (* Get the date from X days back in the aapl.json *)
      let ticker_filename = "src/json/aapl.json" in
      let content =
        Lwt_io.with_file ~mode:Lwt_io.Input ticker_filename Lwt_io.read
      in
      let%lwt json = content >|= Yojson.Basic.from_string in
      match json with
      | `Assoc root_list -> (
          match List.assoc_opt "Time Series (Daily)" root_list with
          | Some (`Assoc time_series_list) ->
              let sorted_dates =
                List.sort
                  (fun (d1, _) (d2, _) -> compare d2 d1)
                  time_series_list
              in
              if List.length sorted_dates > days_back then
                let date, _ = List.nth sorted_dates days_back in
                Lwt.return date
              else Lwt.fail_with "Not enough data for the specified days_back"
          | _ -> Lwt.fail_with "No 'Time Series (Daily)' key found")
      | _ -> Lwt.fail_with "Invalid JSON format"

  let get_ticker_price (query : stock_query) =
    let symbol, days_back = query in
    if days_back = -1 then
      (* Fetch live data using API *)
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
                      match
                        List.assoc_opt most_recent_date time_series_list
                      with
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
    else
      (* Fetch historical data from JSON file *)
      let ticker_filename =
        "src/json/" ^ String.lowercase_ascii symbol ^ ".json"
      in
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input ticker_filename (fun file ->
              Lwt_io.read file >>= fun content ->
              let json = Yojson.Basic.from_string content in
              match json with
              | `Assoc root_list -> (
                  match List.assoc_opt "Time Series (Daily)" root_list with
                  | Some (`Assoc time_series_list) ->
                      let sorted_dates =
                        List.sort
                          (fun (d1, _) (d2, _) -> compare d2 d1)
                          time_series_list
                      in
                      let rec find_price dates days =
                        match (dates, days) with
                        | (_, `Assoc prices) :: _, 0 -> (
                            match List.assoc_opt "4. close" prices with
                            | Some (`String close_price) ->
                                Lwt.return close_price
                            | _ -> Lwt.fail_with "No close price found")
                        | _ :: tail, n -> find_price tail (n - 1)
                        | [], _ -> Lwt.fail_with "Date not found in data"
                      in
                      find_price sorted_dates days_back
                  | _ -> Lwt.fail_with "No 'Time Series (Daily)' key found")
              | _ -> Lwt.fail_with "Invalid JSON format"))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
              Lwt.fail_with "invalid ticker"
          | e -> Lwt.fail (Failure (Printexc.to_string e)))

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
    let corr = cov /. (std_dev1 *. std_dev2) in
    corr

  let rec drop n lst =
    match lst with
    | [] -> []
    | _ :: tail as l -> if n > 0 then drop (n - 1) tail else l

  let get_historical_prices (query : stock_query) days =
    let symbol, days_back = query in
    if days_back = -1 then
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
    else
      (* Fetch historical data from local JSON file *)
      let ticker_filename =
        "src/json/" ^ String.lowercase_ascii symbol ^ ".json"
      in
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input ticker_filename (fun file ->
              Lwt_io.read file >>= fun content ->
              let json = Yojson.Basic.from_string content in
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
                        match (dates, count) with
                        | (_, `Assoc prices) :: tail, n when n > 0 -> (
                            match List.assoc_opt "4. close" prices with
                            | Some (`String close_price) ->
                                extract_closing_prices tail (close_price :: acc)
                                  (n - 1)
                            | _ -> Lwt.fail_with "No close price found")
                        | _ -> Lwt.return (List.rev acc)
                      in
                      (* Adjust the starting point based on days_back *)
                      let start_index =
                        if List.length sorted_dates > days_back then days_back
                        else 0
                      in
                      let starting_dates = drop start_index sorted_dates in
                      extract_closing_prices starting_dates [] days
                  | _ -> Lwt.fail_with "No 'Time Series (Daily)' key found")
              | _ -> Lwt.fail_with "Invalid JSON format"))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
              Lwt.fail_with "invalid ticker"
          | e -> Lwt.fail (Failure (Printexc.to_string e)))

  let get_historical_prices_as_float query days =
    let%lwt prices = get_historical_prices query days in
    Lwt.return (List.map float_of_string prices)

  let get_historical_volumes (query : stock_query) days =
    let symbol, days_back = query in
    if days_back = -1 then
      (* Fetch live data using API *)
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
                      match
                        List.assoc_opt most_recent_date time_series_list
                      with
                      | Some (`Assoc prices) -> (
                          match List.assoc_opt "5. volume" prices with
                          | Some (`String close_price) ->
                              Lwt.return [ close_price ]
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
    else
      (* Fetch historical data from local JSON file *)
      let ticker_filename =
        "src/json/" ^ String.lowercase_ascii symbol ^ ".json"
      in
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input ticker_filename (fun file ->
              Lwt_io.read file >>= fun content ->
              let json = Yojson.Basic.from_string content in
              match json with
              | `Assoc root_list -> (
                  match List.assoc_opt "Time Series (Daily)" root_list with
                  | Some (`Assoc time_series_list) ->
                      let sorted_dates =
                        List.sort
                          (fun (d1, _) (d2, _) -> compare d2 d1)
                          time_series_list
                      in
                      let rec extract_volumes dates acc count =
                        match (dates, count) with
                        | (_, `Assoc prices) :: tail, n when n > 0 -> (
                            match List.assoc_opt "5. volume" prices with
                            | Some (`String volume) ->
                                extract_volumes tail (volume :: acc) (n - 1)
                            | _ -> Lwt.fail_with "No volume data found")
                        | _ -> Lwt.return (List.rev acc)
                      in
                      (* Adjust the starting point based on days_back *)
                      let start_index =
                        if List.length sorted_dates > days_back then days_back
                        else 0
                      in
                      let starting_dates = drop start_index sorted_dates in
                      extract_volumes starting_dates [] days
                  | _ -> Lwt.fail_with "No 'Time Series (Daily)' key found")
              | _ -> Lwt.fail_with "Invalid JSON format"))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
              Lwt.fail_with "invalid ticker"
          | e -> Lwt.fail (Failure (Printexc.to_string e)))

  let get_historical_volumes_as_float query days =
    let%lwt vols = get_historical_volumes query days in
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

  let calculate_stock_correlation query1 query2 days =
    let%lwt prices1 = get_historical_prices_as_float query1 days in
    let%lwt prices2 = get_historical_prices_as_float query2 days in
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

  let get_latest_news_feeds query =
    let symbol, days_back = query in
    if days_back <> -1 then Lwt.fail_with "news is only avaliable in live mode"
    else
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

  let generate_stock_summary query =
    let ticker, _ = query in
    let%lwt historical_prices = get_historical_prices_as_float query 365 in
    let%lwt historical_volumes = get_historical_volumes_as_float query 365 in
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
         "The current price of %s is $%.2f. Comparatively, a year ago, it was \
          trading at $%.2f. This year has been %s for %s, reaching its peak \
          with a high of $%.2f. Notably, the lowest point was with a price of \
          $%.2f. The average trading volume this year was $%.2f."
         ticker current_price year_ago_price descriptor ticker high_price
         low_price average_volume)
end
