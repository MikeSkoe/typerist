open Notty
open Pages
open Ui_infix

module Running = {
    let createElement = (
        ~stats: Stats.t,
        ~typing: Edit.running_typing,
        ~width,
        ~children as _=[],
        ()
    ) => {
        let wpm = Printf.sprintf("WPM: %f", stats.words_per_minute);
        let cps = Printf.sprintf("CPS: %f", stats.chars_per_sec);
        let (target_hd, target_tl) = switch (typing.target) {
            | [] => ("" , [])
            | [head, ...tail] => (head, tail)
        };

        <VList>
            <Text text=cps width />
            <Text text=wpm width pad=(0,0,0,1)/>
            <HList width>
                ...(List.map(
                    text => <Text
                        text
                        width
                        style=A.(fg(gray(10)))
                        pad=(typing.typed == [] ? (0,0,0,0) : (0,1,0,0))
                    />,
                    typing.typed
                )
                @ [<Text
                    text=target_hd
                    width
                    style=A.(fg(black) <+> bg(white))
                    pad=(0,0,0,0)
                /> ]
                @ List.map(
                    text => <Text text width pad=(1,0,0,0)/>,
                    target_tl
                ))
            </HList>
            <Text text=typing.current width/>
        </VList>;
    };
};

module Done = {
    let createElement = (
        ~stats: Stats.t,
        ~width,
        ~children as _=[],
        ()
    ) => {
        let wpm = Printf.sprintf("WPM: %f", stats.words_per_minute);
        let cps = Printf.sprintf("CPS: %f", stats.chars_per_sec);

        <VList>
            <Text text=cps width />
            <Text text=wpm width pad=(0,0,0,1)/>
        </VList>
    };
};

let createElement = (
    ~model: Edit.model,
    ~width,
    ~children as _=[],
    ()
) => {
    switch (model.typing) {
        | Running(typing) => <Running
            stats=model.stats
            typing=typing
            width
        />
        | Done => <Done
            stats=model.stats
            width
        />
    };
};
