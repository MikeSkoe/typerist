let term = Notty_lwt.Term.create ();

let draw = (model: Model.model) => {
      let cps = model.stats.chars_per_sec
            |> string_of_float
            |> (++)("cps: ");
      let wpm = model.stats.words_per_minute
            |> string_of_float
            |> (++)("wpm: ");

      let (width, _height) = Notty_lwt.Term.size(term);
      let image =
            <VList>
                  <Text text={cps} width />
                  <Text text={wpm} width pad=(0,0,0,1)/>
                  (switch (model.page) {
                  | Model.Edit(page) => <Edit model=page width/>
                  | Model.Menu(page) => <Menu model=page width/>
                  })
            </VList>
      Notty_lwt.Term.image(term, image);
}

