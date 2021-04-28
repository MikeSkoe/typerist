let term = Notty_lwt.Term.create ();

let draw = (model: Model.model) => {
      let (width, _height) = Notty_lwt.Term.size(term);
      let image =
            switch (model.page) {
            | Model.Edit(page) => <Edit model=page width/>
            | Model.Menu(page) => <Menu model=page width/>
            };
      Notty_lwt.Term.image(term, image);
}

