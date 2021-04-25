open Lwt.Infix

type direction =
      | Up
      | Down

type msg =
      | Tick
      | Shift of direction

type menu_option =
      | First
      | Second
      | Third

type model = {
      active: menu_option
}

let empty = {
      active = First
}

let shift direction model =
      match model.active, direction with
      | Second, Up | First, Up -> { active = First }
      | First, Down | Third, Up -> { active = Second }
      | Second, Down | Third, Down -> { active = Third }

let get_event term =
      (Notty_lwt.Term.events term |> Lwt_stream.get) >|=
      function
      | Some (`Key (`Arrow `Up, _)) -> `Menu (Shift Up)
      | Some (`Key (`Arrow `Down, _)) -> `Menu (Shift Down)
      | _ -> `Exit

let get_tick () = Lwt_unix.sleep 1.0 >|= fun () -> `Menu Tick

let update term msg model =
      match msg with
      | Tick ->
            model
            , `Right (get_tick ())
      | Shift direction ->
            shift direction model
            , `Left (get_event term)
