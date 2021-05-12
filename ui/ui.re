let term = Notty_lwt.Term.create ();

let draw = (model: Model.model) => {
      let wpm = Printf.sprintf("Best WPM: %d", model.stats.words_per_minute);
      let cps = Printf.sprintf("Best CPS: %d", model.stats.chars_per_sec);

      let (width, _height) = Notty_lwt.Term.size(term);
      let image =
            <VList>
                  <Text text={cps} />
                  <Text text={wpm} pad=(0,0,0,1)/>
                  (switch (model.page) {
                  | Model.Edit(page) => <Edit model=page width/>
                  | Model.Menu(page) => <Menu model=page/>
                  })
            </VList>
      Notty_lwt.Term.image(term, image);
}

