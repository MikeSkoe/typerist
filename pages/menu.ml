open Lwt.Infix

type direction =
      | Up
      | Down

type msg =
      | Tick
      | Shift of direction

type menu_option =
      | Start
      | Exit

type model = {
      active: menu_option
}

let empty () = {
      active = Start
}

let shift direction model =
      match model.active, direction with
      | Start, Up | Exit, Up -> { active = Start }
      | Start, Down | Exit, Down -> { active = Exit }

let get_event term model =
      (Notty_lwt.Term.events term |> Lwt_stream.get) >|=
      function
      | Some (`Key (`Arrow `Up, _)) -> `Menu (Shift Up)
      | Some (`Key (`Arrow `Down, _)) -> `Menu (Shift Down)
      | Some (`Key (`Enter, _)) ->
            begin match model.active with
            | Start -> `Navigation Navigation.ToEdit
            | Exit -> `Navigation Navigation.Exit
            end
      | _ -> `Navigation Navigation.Exit

let get_tick () = Lwt_unix.sleep 1.0 >|= fun () -> `Menu Tick

let update term model msg lmsg rmsg =
      match msg with
      | Tick ->
            let rmsg = get_tick () in
            (model, lmsg, rmsg)
      | Shift direction ->
            let model = shift direction model in
            let lmsg = get_event term model in
            (model, lmsg, rmsg)
