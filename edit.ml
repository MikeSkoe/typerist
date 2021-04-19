open Lwt.Infix

type msg =
      | Stop
      | Tick
      | Key of char

type model = {
      time: int;
      last_char: char;
}

let empty = {
      time = 0;
      last_char = ' ';
}

let update term msg model =
      match msg with
      | Stop ->
            model
            , `Exit
      | Tick -> 
            { model with time = model.time + 1 }
            , `Right (Lwt_unix.sleep 1.0 >|= fun () -> `Edit Tick)
      | Key chr ->
            { time = model.time + 2; last_char = chr }
            , `Left ((Notty_lwt.Term.events term |> Lwt_stream.get) >|=
                  function
                  | Some (`Key (`ASCII chr, _)) -> `Edit (Key chr)
                  | _ -> `Edit Stop
            )