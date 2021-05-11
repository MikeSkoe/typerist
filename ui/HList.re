open Notty

// TESTS!
let get_line = (imgs: list(I.t), width: int) => {
    let rec iter = (img, imgs) => switch (imgs) {
        | [] => (img, imgs)
        | [head, ...tail] =>
            let new_head = I.(img <|> head);
            let new_width = I.(width(new_head));

            if (new_width >= width) {
                (img, imgs);
            } else {
                iter(new_head, tail);
            }
    };

    iter(I.empty, imgs);
}

// TESTS!
let rec fit_imgs = (l_imgs: list(I.t), r_imgs: list(I.t), width: int) => {
    switch ((l_imgs, r_imgs)) {
        | ([], r_imgs) => r_imgs
        | ([l_head, ...l_tail], []) => fit_imgs(l_tail, [l_head], width)
        | (left, right) => {
            let (line, rest) = get_line(left, width);

            if (rest == []) {
                right @ [line];
            } else {
                fit_imgs(rest, right @ [line], width);
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
