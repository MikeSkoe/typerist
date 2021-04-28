open Notty
open Ui_infix

let createElement = (
    ~width,
    ~model: Pages.Menu.model,
    ~children as _=[],
    ()
) => {
    let active_if = (opt) =>
        (opt === model.active)
            ? A.(bg(white) <+> fg(black))
            : A.empty;
    <VList>
        <Text
            text="start"
            style=(active_if(Pages.Menu.Start))
            width
        />
        <Text
            text="exit"
            style=(active_if(Pages.Menu.Exit))
            width
        />
    </VList>;
};
