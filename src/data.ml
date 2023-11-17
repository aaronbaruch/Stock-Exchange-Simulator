open Cohttp_lwt_unix
open Lwt

module type Data = sig
  val get_ticker_price : string -> string Lwt.t
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
end
