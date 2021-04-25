module Term = Notty_unix.Term
open Lwt.Infix
open Lwt.Syntax
open Pages

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
            | `Both (lmsg, rmsg) -> loop (lmsg, rmsg, model)
      in
      loop (
            Edit.get_event Ui.term,
            Edit.get_tick (),
            Model.empty
      )

let () = Lwt_main.run @@ main_lwt ()
