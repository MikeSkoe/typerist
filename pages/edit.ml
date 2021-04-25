open Lwt.Infix

type msg =
      | Tick
      | Key of char
      | Backspace
      | Resize

type model = {
      time: int;
      typed: string list;
      current: string;
      target: string list;
}

let empty = {
      time = 0;
      typed = [];
      current = "";
      target = String.split_on_char ' ' "this is the text to type";
}

let tick model = {
      model with
      time = model.time + 1;
}

let type_char chr model =
      match chr, model.current with
      | (' ', "") -> model
      | (' ', current) ->
            begin match model.target with
            | target_hd :: taret_tl when current = target_hd -> {
                  model with
                  current = ""; 
                  typed = model.typed @ [current];
                  target = taret_tl;
            }
            | _ -> model
            end
      | (chr, current) -> {
            model with
            current = current ^ Char.escaped chr;
      }

let backspace model =
      match model.current with
      | ""  -> model
      | current -> {
            model with
            current = String.sub current 0 String.(length current - 1)
      }

let get_event term =
      (Notty_lwt.Term.events term |> Lwt_stream.get) >|=
      function
      | Some (`Key (`ASCII chr, _)) -> `Edit (Key chr)
      | Some (`Key (`Backspace, _)) -> `Edit Backspace
      | Some (`Resize _) -> `Edit Resize
      | _ -> `Navigation Navigation.ToMenu

let get_tick () = Lwt_unix.sleep 1.0 >|= fun () -> `Edit Tick

let update term msg model =
      match msg with
      | Resize ->
            model
            , `Left (get_event term)
      | Tick -> 
            tick model
            , `Right (get_tick ())
      | Backspace ->
            backspace model
            , `Left (get_event term)
      | Key chr ->
            type_char chr model
            , `Left (get_event term)
