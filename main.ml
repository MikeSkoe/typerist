module Term = Notty_unix.Term
open Lwt.Infix
open Pages

let main_lwt () =
      let rec loop model lmsg rmsg =
            Ui.draw model >>= fun _ ->
            Model.update Ui.term loop model lmsg rmsg
      in
      loop 
            Model.(empty ())
            Menu.(get_event Ui.term Menu.(empty ()))
            Menu.(get_tick ())

let () = Lwt_main.run @@ main_lwt ()
