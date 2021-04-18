open Notty
module Term = Notty_unix.Term

let rec repeat str =
      function
      | 0 -> str
      | num -> repeat (str ^ str) (num - 1)

module State = struct
      type model = {
            text: string;
            width: int;
      }

      type msg =
            | Resize of int * int
            | Quit
            | NoOp

      let initial_state = {
            text = "one two three four five six seven eight nine ten eleven twelve";
            width = 100
      }

      let update (model, msg) =
            let model =
                  match msg with
                  | Resize (w, _h) -> { model with width = w }
                  | Quit -> model
                  | NoOp -> model
            in
            (model, msg)
end

module TextUI = struct
      let fit_string str width =
            let split_words = String.split_on_char ' ' in
            let concat_to_lines = List.fold_left
                  (fun acc word ->
                        match acc with
                        | head :: tail ->
                              if ((String.length head) + (String.length word) + 1) > width
                              then word :: head :: tail
                              else (head ^ " " ^ word) :: tail
                        | [] -> [word]
                  ) 
                  []
            in 
            str
                  |> split_words
                  |> concat_to_lines
                  |> List.rev

      let i ~text ~width =
            fit_string text width
                  |> List.map I.(string A.empty)
                  |> List.fold_left I.(<->) I.empty
end

module UI = struct
      let term = Term.create ()

      let update ((model, _msg): State.model * State.msg) =
            let text = TextUI.i ~text:model.text ~width:model.width in
            text
                  |> Term.image term;

            let msg =
                  match Term.event term with
                  | `Resize (w, h) -> State.Resize (w, h)
                  | _ -> State.Quit
            in
            (model, msg)
end

let main () =
      let rec loop (model, msg) =
            match msg with
            | State.Quit -> ()
            | _ -> (model, msg)
                  |> State.update
                  |> UI.update
                  |> loop in

      loop (State.initial_state, State.NoOp)

let _ = main ()

