open Pages

type msg = [
      | `Edit of Edit.msg
      | `Menu of Menu.msg
]

type page =
      | Edit of Edit.model
      | Menu of Menu.model

type model = {
      user: string;
      page: page;
}

let empty = {
      user = "<USER NAME>";
      page = Edit Edit.empty
}

type thread = [
      | `Exit
      | `Left of msg Lwt.t
      | `Right of msg Lwt.t
]

let update term msg model =
      let page, thread =
            match msg with
            | `Exit ->
                  model.page,
                  `Exit
            | `Edit msg ->
                  begin match model.page with 
                  | Edit page -> 
                        let (page, thread) = Edit.update term msg page in
                        Edit page, thread
                  | _ ->
                        let (page, thread) = Edit.update term msg Edit.empty in
                        Edit page, thread
                  end
            | `Menu msg ->
                  begin match model.page with
                  | Menu page ->
                        let (page, thread) = Menu.update term msg page in
                        Menu page, thread
                  | _ ->
                        let (page, thread) = Menu.update term msg Menu.empty in
                        Menu page, thread
                  end
      in
      { model with page = page }
      , thread
