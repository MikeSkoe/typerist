open Pages
open Lwt.Infix

type msg = [
      | `Navigation of Navigation.msg
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

let empty () = {
      user = "<USER NAME>";
      page = Menu Menu.(empty ())
}

let update term loop model lmsg rmsg  =
      (lmsg <?> rmsg) >>=
      function
      | `Navigation msg ->
            begin match msg with
            | Navigation.Exit ->
                  Lwt.return_unit
            | Navigation.ToEdit ->
                  loop 
                        { model with page = Edit Edit.(empty ()) }
                        Edit.(get_string ())
                        Edit.(get_never ())
            | Navigation.ToMenu ->
                  loop
                        { model with page = Menu Menu.(empty ()) }
                        Menu.(get_event term Menu.(empty ()))
                        Menu.(get_tick ())
            end
      | `Edit msg ->
            begin match model.page with 
            | Edit page ->
                  let page, lmsg, rmsg = Edit.update term page msg lmsg rmsg in
                  loop
                        { model with page = Edit page }
                        lmsg
                        rmsg
            | _ ->
                  let page, lmsg, rmsg = Edit.update term Edit.(empty ()) msg lmsg rmsg in
                  loop
                        { model with page = Edit page }
                        lmsg
                        rmsg
            end
      | `Menu msg ->
            begin match model.page with
            | Menu page ->
                  let page, lmsg, rmsg = Menu.update term page msg lmsg rmsg  in
                  loop
                        { model with page = Menu page }
                        lmsg
                        rmsg
            | _ ->
                  let page, lmsg, rmsg = Menu.update term Menu.(empty ()) msg lmsg rmsg in
                  loop
                        { model with page = Menu page }
                        lmsg
                        rmsg
            end

