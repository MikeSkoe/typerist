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
        let wpm = Printf.sprintf("WPM: %d", stats.words_per_minute);
        let cps = Printf.sprintf("CPS: %d", stats.chars_per_sec);
        let (target_hd, target_tl) = switch (typing.target) {
            | [] => ("" , [])
            | [head, ...tail] => (head, tail)
        };

        <VList>
            <Text text=cps />
            <Text text=wpm pad=(0,0,0,1)/>
            <HList width>
                ...(List.map(
                    text => <Text
                        text
                        style=A.(fg(gray(10)))
                        pad=(typing.typed == [] ? (0,0,0,0) : (0,1,0,0))
                    />,
                    typing.typed
                )
                @ List.map(
                    text => <Text
                        text
                        style=(
                            Edit.is_correct(typing)
                            ? A.(fg(black) <+> bg(white))
                            : A.(fg(black) <+> bg(red))
                        )
                        pad=(0,0,0,0)
                    />,
                    [target_hd]
                ) 
                @ List.map(
                    text => <Text text pad=(1,0,0,0)/>,
                    target_tl
                ))
            </HList>
            <Text text=typing.current />
        </VList>;
    };
};

module Done = {
    let createElement = (
        ~stats: Stats.t,
        ~children as _=[],
        ()
    ) => {
        let wpm = Printf.sprintf("WPM: %d", stats.words_per_minute);
        let cps = Printf.sprintf("CPS: %d", stats.chars_per_sec);

        <VList>
            <Text text=cps />
            <Text text=wpm pad=(0,0,0,1)/>
            <Text text="Press any key to go to menu" style=A.(fg(gray(10))) pad=(0,0,1,1)/>
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
        />
    };
};
