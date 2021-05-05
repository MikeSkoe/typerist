open Lwt.Infix

type msg =
      | Tick
      | Key of char
      | Backspace
      | Resize
      | SetString of string

type typing =
      | Running of {
            typed: string;
            current: string;
            target: string list;
      }
      | Done

type model = {
      char_per_sec: int;
      initial_text: string;
      initial_time: int;
      typing: typing;
}

let initial_text = "Fake loading..."

let empty () = {
      initial_text = initial_text;
      initial_time = Unix.time () |> int_of_float;
      char_per_sec = 0;
      typing = Running {
            typed = "";
            current = "";
            target = String.split_on_char ' ' initial_text;
      }
}

let get_char_per_sec initial_time typed = 
      let chars = String.length typed in
      let curr_time = Unix.time () |> int_of_float in
      let ellapsed = curr_time - initial_time in
      chars / ellapsed

let tick model =
      match model.typing with
      | Running typing -> {
            model with
            char_per_sec = get_char_per_sec model.initial_time typing.typed;
      }
      | Done -> model

let type_char chr model =
      match model.typing with
      | Running typing ->
            begin match chr, typing.current with
            | (' ', "") -> model
            | (' ', current) ->
                  begin match typing.target with
                  | target_hd :: target_tl when current = target_hd -> {
                        model with
                        typing = Running {
                              current = ""; 
                              typed =
                                    if typing.typed = ""
                                    then current
                                    else typing.typed ^ " " ^ current;
                              target = target_tl;
                        }
                  }
                  | _ -> model
                  end
            | (chr, current) -> {
                  model with
                  typing =
                        let new_current = current ^ String.make 1 chr in
                        if List.length typing.target = 1 && List.hd typing.target = new_current
                        then Done
                        else Running { typing with current = new_current }
            }
            end
      | Done -> model

let backspace model =
      match model.typing with
      | Running typing ->
            begin match typing.current with
            | ""  -> model
            | current -> {
                  model with
                  typing = Running {
                        typing with
                        current = String.sub current 0 String.(length current - 1)
                  }
            }
            end
      | Done -> model

let set_string str model =
      match model.typing with
      | Running typing -> {
            model with
            typing = Running {
                  typing with
                  target = String.split_on_char ' ' str;
            }
      }
      | Done -> model

let get_event term typing =
      (Notty_lwt.Term.events term |> Lwt_stream.get) >|= fun event ->
      match typing, event with
      | Done, Some (`Key (`Backspace, _)) -> `Navigation Navigation.ToMenu
      | _, Some (`Key (`ASCII chr, _)) -> `Edit (Key chr)
      | _, Some (`Key (`Backspace, _)) -> `Edit Backspace
      | _, Some (`Resize _) -> `Edit Resize
      | _, _ -> `Navigation Navigation.ToMenu

let get_to_menu () = Lwt.return @@ `Navigation Navigation.ToMenu

let get_string () =
      let open Cohttp_lwt_unix in
      let open Yojson.Basic in
      let to_string = Cohttp_lwt.Body.to_string in
      let request =
            "https://api.adviceslip.com/advice"
            |> Uri.of_string
            |> Client.get
      in 
      request >>= fun (_, body) ->
      to_string body >>= fun body ->

      let json = from_string body in
      let slip =
            json
            |> Util.member "slip"
            |> Util.member "advice"
            |> Util.to_string
      in
      Lwt.return @@ `Edit (SetString slip)

let get_tick () = Lwt_unix.sleep 1.0 >|= fun () -> `Edit Tick
let get_never () = Lwt_unix.sleep 999.0 >|= fun () -> `Edit Tick

let update term model msg lmsg rmsg =
      match msg with
      | Resize ->
            let lmsg = get_event term model.typing in
            (model, lmsg, rmsg)
      | Tick -> 
            let model = tick model in
            let rmsg = get_tick () in
            (model, lmsg, rmsg)
      | Backspace ->
            let model = backspace model in
            let lmsg = get_event term model.typing in
            (model, lmsg, rmsg)
      | Key chr ->
            let model = type_char chr model in
            let lmsg, rmsg =
                  begin match model.typing with
                  | Running typing ->
                        if String.equal typing.typed model.initial_text
                        then (get_never (), get_never ())
                        else (get_event term model.typing, rmsg)
                  | Done -> (get_event term model.typing, get_never ())
                  end
            in
            (model, lmsg, rmsg)
      | SetString str ->
            let model = set_string str model in
            let lmsg = get_event term model.typing in
            let rmsg = get_tick () in
            (model, lmsg, rmsg)
