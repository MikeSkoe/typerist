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
      stats: Stats.t;
      page: page;
}

let empty () = {
      stats = Stats.empty;
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
            | Navigation.SaveResult stats ->
                  let page = Menu Menu.(empty ()) in
                  let stats = Stats.make
                        (max stats.chars_per_sec model.stats.chars_per_sec)
                        (max stats.words_per_minute model.stats.words_per_minute)
                  in
                  loop
                        { stats; page; }
                        Menu.(get_event term Menu.(empty ()))
                        Menu.(get_tick ())
            end
      | `Edit msg ->
            let page =
                  begin match model.page with 
                  | Edit page -> page
                  | _ -> Edit.empty ()
                  end
            in
            let page, lmsg, rmsg = Edit.update term page msg lmsg rmsg in
            loop
                  { model with page = Edit page }
                  lmsg
                  rmsg
      | `Menu msg ->
            let page =
                  begin match model.page with
                  | Menu page -> page
                  | _ ->  Menu.empty ()
                  end
            in
            let page, lmsg, rmsg = Menu.update term page msg lmsg rmsg  in
            loop
                  { model with page = Menu page }
                  lmsg
                  rmsg

