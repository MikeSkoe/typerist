open Notty

let fit_string = (str, width) => {
    let split_words = String.split_on_char(' ');
    let concat_to_lines =
        []
        |> List.fold_left ((acc, word) => switch (acc) {
            | [] => [word]
            | [head, ...tail] =>
                ((String.length(head) + String.length(word) + 1) > width)
                    ?  [word, head, ...tail]
                    : [(head ++ " " ++ word), ...tail]
        });
    str
        |> split_words
        |> concat_to_lines
        |> List.rev;
};

let createElement = (
    ~text,
    ~width,
    ~style=A.empty,
    ~pad as (l,r,t,b)=(0,0,0,0),
    ~children as _=[],
    ()
) => 
    fit_string(text, width)
        |> List.map (I.string(style))
        |> List.map (I.pad(~l, ~r, ~t, ~b))
        |> List.fold_left(I.(<->), I.empty);
