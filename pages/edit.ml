open Lwt.Infix

type msg =
      | Tick
      | Key of char
      | Backspace
      | Resize
      | SetString of string

type model = {
      initial_text: string;
      initial_time: int;
      char_per_sec: int;
      typed: string;
      current: string;
      target: string list;
}

let initial_text = "This is some long text, just to test your typing abbiliry..."

let empty () = {
      initial_text = initial_text;
      initial_time = Unix.time () |> int_of_float;
      char_per_sec = 0;
      typed = "";
      current = "";
      target = String.split_on_char ' ' initial_text;
}

let get_char_per_sec initial_time typed = 
      let chars = String.length typed in
      let curr_time = Unix.time () |> int_of_float in
      let ellapsed = curr_time - initial_time in
      chars / ellapsed

let tick model = {
      model with
      char_per_sec = get_char_per_sec model.initial_time model.typed;
}

let type_char chr model =
      match chr, model.current with
      | (' ', "") -> model
      | (' ', current) ->
            begin match model.target with
            | target_hd :: taret_tl when current = target_hd -> {
                  model with
                  current = ""; 
                  typed =
                        if model.typed = ""
                        then current
                        else model.typed ^ " " ^ current;
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

let set_string str model = {
      model with
      target = String.split_on_char ' ' str;
}

let get_event term =
      (Notty_lwt.Term.events term |> Lwt_stream.get) >|=
      function
      | Some (`Key (`ASCII chr, _)) -> `Edit (Key chr)
      | Some (`Key (`Backspace, _)) -> `Edit Backspace
      | Some (`Resize _) -> `Edit Resize
      | _ -> `Navigation Navigation.ToMenu

let get_string () =
      let open Cohttp_lwt_unix in
      let open Yojson.Basic in
      let reques = "https://api.adviceslip.com/advice"
            |> Uri.of_string
            |> Client.get
      in 
      reques >>= fun (_, body) ->
      Cohttp_lwt.Body.to_string body >>= fun body ->

      let json = from_string body in
      let slip = json |> Util.member "slip" |> Util.member "advice" |> to_string in
      Lwt.return @@ `Edit (SetString slip)

let get_tick () = Lwt_unix.sleep 1.0 >|= fun () -> `Edit Tick
let get_deferred () = Lwt_unix.sleep 999.0 >|= fun () -> `Edit Tick

let update term model msg lmsg rmsg =
      match msg with
      | Resize ->
            let lmsg = get_event term in
            model
            , lmsg
            , rmsg
      | Tick -> 
            let model = tick model in
            let rmsg = get_tick () in
            model
            , lmsg
            , rmsg
      | Backspace ->
            let model = backspace model in
            let lmsg = get_event term in
            model
            , lmsg 
            , rmsg
      | Key chr ->
            let model = type_char chr model in
            let lmsg = get_event term in
            let rmsg = 
                  if model.typed = model.initial_text
                  then get_deferred ()
                  else rmsg
            in
            model
            , lmsg
            , rmsg
      | SetString str ->
            let model = set_string str model in
            let lmsg = get_event term in
            let rmsg = get_tick () in
            model
            , lmsg
            , rmsg