open Notty

let rec fit_imgs = (l_imgs: list(I.t), r_imgs: list(I.t), width: int) => {
    switch ((l_imgs, r_imgs)) {
        | ([], r_imgs) => List.rev(r_imgs)
        | ([l_head, ...l_tail], []) => fit_imgs(l_tail, [l_head], width)
        | ([l_head, ...l_tail], [r_head, ...r_tail]) => {
            let head = I.(r_head <|> l_head);
            let head_width = I.(width(head));

            if (head_width < width) {
                fit_imgs(l_tail, [head, ...r_tail], width);
            } else {
                fit_imgs(l_tail, [l_head, r_head, ...r_tail], width);
            }
        }
    }
};

let createElement = (
    ~children=[],
    ~width,
    ()
) => {
    List.fold_left(I.(<->), I.empty, fit_imgs(children, [], width));
};
