open Notty
module Term = Notty_unix.Term

let rec repeat str =
      function
      | 0 -> str
      | num -> repeat (str ^ str) (num - 1)

module State = struct
      type model = {
            target: string;
            typed: string;
            current_word: string;
            width: int;
      }

      type msg =
            | Resize of int * int
            | TypeChar of char
            | Quit
            | NoOp

      let initial_state = {
            target = "one two three four five six seven eight nine ten eleven twelve";
            typed = "";
            current_word = "";
            width = 100
      }

      let update (model, msg) =
            let model =
                  match msg with
                  | Resize (w, _h) -> { model with width = w }
                  | Quit -> model
                  | NoOp -> model
                  | TypeChar ch ->
                        match ch with
                        | ' ' ->
                              let space = if model.typed = "" then "" else " " in
                              { model with
                                    typed = model.typed ^ space ^ model.current_word;
                                    current_word = "";
                              }
                        | ch ->
                              let char_str = Char.escaped ch in
                              { model with
                                    current_word = (model.current_word ^ char_str);
                              }
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
            let target = TextUI.i ~text:model.target ~width:model.width in
            let typed = TextUI.i ~text:model.typed ~width:model.width in
            let current_word = TextUI.i ~text:model.current_word ~width:model.width in
            I.(target <-> typed <-> current_word) |> Term.image term;

            let msg =
                  match Term.event term with
                  | `Resize (w, h) -> State.Resize (w, h)
                  | `Key ((`ASCII ch), _) -> State.TypeChar ch
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

