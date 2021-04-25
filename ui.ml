open Notty
open Pages

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

      let make text ~width ?style:(style=A.empty) ?pad:((l,r,t,b)=(0,0,0,0)) () =
            fit_string text width
                  |> List.map I.(string style)
                  |> List.map I.(pad ~l ~r ~t ~b)
                  |> List.fold_left I.(<->) I.empty
end

let term = Notty_lwt.Term.create ()

let menu (model: Menu.model) =
      let term_width, _height = Notty_lwt.Term.size term in
      let active_if option =
            if option = model.active
            then A.(bg white ++ fg black)
            else A.empty
      in
      I.(
            TextUI.make "first"
                  ~style:(active_if Menu.First)
                  ~width:term_width ()
            <-> TextUI.make "second"
                  ~style:(active_if Menu.Second)
                  ~width:term_width ()
            <-> TextUI.make "third"
                  ~style:(active_if Menu.Third)
                  ~width:term_width ()
      )

let edit (model: Edit.model) =
      let term_width, _height = Notty_lwt.Term.size term in
      let time = Printf.(sprintf "time: %d" model.time) in
      let typed = String.(concat " " model.typed) in
      let target_hd, target_tl =
            match model.target with
            | [] ->
                  ""
                  , ""
            | head :: tail ->
                  head
                  , if tail = []
                  then ""
                  else String.concat " " tail
      in
      I.(
            TextUI.make time
                  ~width:term_width
                  ~pad:(0,0,0,1) ()
            <-> (
                  (
                  if typed = ""
                  then empty
                  else TextUI.make typed
                        ~width:term_width
                        ~style:A.(fg @@ gray 10)
                        ~pad:(0,1,0,0)()
                  )
                  <|> TextUI.make target_hd
                        ~width:term_width
                        ~style:A.(fg black ++ bg white)
                        ~pad:(0,1,0,0) ()
                  <|> TextUI.make target_tl
                        ~width:term_width ()
            )
            <-> TextUI.make model.current
                  ~width:term_width ()
      )
      
let draw (model: Model.model) =
      let image =
            match model.page with
            | Model.Edit page -> edit page
            | Model.Menu page -> menu page
      in

      Notty_lwt.Term.image term image;

