open Notty

let term = Notty_lwt.Term.create ()

let draw (model: Model.model) =
      let image =
            match model.page with
            | Model.Edit edit_page ->
                  I.(string A.empty ("time: "  ^ string_of_int edit_page.time))
      in
      Notty_lwt.Term.image term image;