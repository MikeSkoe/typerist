type msg = [
      | `Edit of Edit.msg
]

type page =
      | Edit of Edit.model

type model = {
      user: string;
      page: page;
}

let empty = {
      user = "<USER NAME>";
      page = Edit Edit.empty
}

type thread = [
      | `NOOP
      | `Exit
      | `Left of msg Lwt.t
      | `Right of msg Lwt.t
]

let update term =
      let update_edit = Edit.update term in

      fun msg model ->
            let (page, thread) =
                  match msg, model.page with
                  | `Edit edit_msg, Edit page ->
                        let (page, thread) = update_edit edit_msg page in
                        Edit page, thread
                  | _ -> model.page, `NOOP
            in
            { model with page = page }
            , thread

