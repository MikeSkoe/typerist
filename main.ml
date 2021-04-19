module Term = Notty_unix.Term
open Lwt.Infix
open Lwt.Syntax

(*
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

      let update ((model, _msg): Edit.model * Edit.msg) =
            let typed = String.concat " " model.typed in
            let source = String.concat " " model.source in
            let current = model.current in
            I.(
                  TextUI.i ~text:typed ~width:model.width
                  <-> TextUI.i ~text:source ~width:model.width
                  <-> TextUI.i ~text:current ~width:model.width
            ) |> Term.image term;

            let msg =
                  match Term.event term with
                  | `Resize (w, h) -> Edit.Resize (w, h)
                  | `Key ((`ASCII ch), _) -> Edit.TypeChar ch
                  | _ -> Edit.Quit
            in
            (model, msg)
end
*)

let main_lwt () =
      let update = Model.update Ui.term in

      let rec loop (lmsg, rmsg, model) =
            let* _ = Ui.draw model in
            let* msg = (lmsg <?> rmsg) in
            let model, thread_msg = update msg model in
            match thread_msg with
            | `Exit -> Lwt.return_unit
            | `Left lmsg -> loop (lmsg, rmsg, model)
            | `Right rmsg -> loop (lmsg, rmsg, model)
            | `NOOP -> loop (lmsg, rmsg, model)
      in
      loop (
            (Lwt.return @@ `Edit (Edit.Key 'c')),
            (Lwt.return @@ `Edit Edit.Tick),
            Model.empty
      )

let () = Lwt_main.run @@ main_lwt ()

(* let main () =
      let rec loop (model, msg) =
            match msg with
            | Edit.Quit -> ()
            | _ -> (model, msg)
                  |> Edit.update
                  |> UI.update
                  |> loop in

      loop (Edit.initial_state, Edit.NoOp) *)
