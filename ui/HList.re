open Notty

let createElement = (
    ~children=[],
    ()
) => {
    List.fold_left(I.(<|>), I.empty, children);
};
