open Lwt.Infix

type msg =
      | Tick
      | Key of char
      | Backspace
      | Resize
      | SetString of string

type running_typing = {
      typed: string list;
      current: string;
      target: string list;
}

type typing =
      | Running of running_typing
      | Done

type model = {
      stats: Stats.t;
      initial_text: string;
      initial_time: int;
      typing: typing;
}

let is_correct typing =
      match typing.target, typing.current with
      | [], _ -> true
      | _, "" -> true
      | head :: _, current ->
            let len = min
                  (String.length head)
                  (String.length current)
            in
            let sub_head = String.sub head 0 len in

            String.equal current sub_head

let initial_text = "Fake loading..."

let empty () = {
      stats = Stats.empty;
      initial_text = initial_text;
      initial_time = Unix.time () |> int_of_float;
      typing = Running {
            typed = [];
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
      | Running typing ->
            let curr_time = Unix.time () |> int_of_float in
            let ellapsed = curr_time - model.initial_time in
            let stats = Stats.update typing.typed ellapsed in
            {
                  model with
                  stats = stats;
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
                                    if typing.typed = []
                                    then [current]
                                    else typing.typed @ [current];
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

let get_event term model =
      (Notty_lwt.Term.events term |> Lwt_stream.get) >|= fun event ->
      match model.typing, event with
      | Done, Some (`Key _) ->
            `Navigation (Navigation.SaveResult model.stats)
      | _, Some (`Key (`ASCII chr, _)) -> `Edit (Key chr)
      | _, Some (`Key (`Backspace, _)) -> `Edit Backspace
      | _, Some (`Resize _) -> `Edit Resize
      | _, _ -> `Navigation Navigation.ToMenu

let get_to_menu () = Lwt.return @@ `Navigation Navigation.ToMenu

let get_string () =
      (Api.get_random_text ()) >>= fun text ->
      Lwt.return @@ `Edit (SetString text)

let get_tick () = Lwt_unix.sleep 1.0 >|= fun () -> `Edit Tick
let get_never () = Lwt_unix.sleep 999.0 >|= fun () -> `Edit Tick

let update term model msg lmsg rmsg =
      match msg with
      | Resize ->
            let lmsg = get_event term model in
            (model, lmsg, rmsg)
      | Tick -> 
            let model = tick model in
            let rmsg = get_tick () in
            (model, lmsg, rmsg)
      | Backspace ->
            let model = backspace model in
            let lmsg = get_event term model in
            (model, lmsg, rmsg)
      | Key chr ->
            let model = type_char chr model in
            let lmsg, rmsg =
                  begin match model.typing with
                  | Running typing ->
                        if String.equal String.(concat " " typing.typed) model.initial_text
                        then (get_never (), get_never ())
                        else (get_event term model, rmsg)
                  | Done -> (get_event term model, get_never ())
                  end
            in
            (model, lmsg, rmsg)
      | SetString str ->
            let model = set_string str model in
            let lmsg = get_event term model in
            let rmsg = get_tick () in
            (model, lmsg, rmsg)
