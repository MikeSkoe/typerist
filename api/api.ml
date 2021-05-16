open Lwt.Infix

let get_random_text () =
      let open Cohttp_lwt_unix in
      let open Yojson.Basic in

      let request = Uri.of_string "https://api.adviceslip.com/advice" in 
      let requests = [Client.get request; Client.get request; Client.get request] in
      let string_of_request (_, body) = Cohttp_lwt.Body.to_string body in

      Lwt.all requests >>= fun reqs ->
      Lwt.all (List.map string_of_request reqs) >>= fun bodies ->

      let text = bodies
            |> List.map (fun body -> 
                  body
                  |> from_string
                  |> Util.member "slip"
                  |> Util.member "advice"
                  |> Util.to_string
            )
            |> String.concat " "
      in
      Lwt.return text

