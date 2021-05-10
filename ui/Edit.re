open Notty
open Pages
open Ui_infix

module Running = {
    let createElement = (
        ~words_per_minute: float,
        ~chars_per_sec: float,
        ~typed: string,
        ~current: string,
        ~target: list(string),
        ~width,
        ~children as _=[],
        ()
    ) => {
        let wpm = Printf.sprintf("WPM: %f", words_per_minute);
        let cps = Printf.sprintf("CPS: %f", chars_per_sec);
        let (target_hd, target_tl) = switch (target) {
            | [] => ("" , "")
            | [head, ...tail] => (
                head,
                if (tail === []) {
                    "";
                } else {
                    String.concat(" ", tail);
                }
            )
        };
        <VList>
            <Text text=cps width />
            <Text text=wpm width pad=(0,0,0,1)/>
            <HList>
                <Text
                    text=typed
                    width
                    style=A.(fg(gray(10)))
                    pad=(typed == "" ? (0,0,0,0) : (0,1,0,0))
                />
                <Text
                    text=target_hd
                    width
                    style=A.(fg(black) <+> bg(white))
                    pad=(0,1,0,0)
                />
                <Text text=target_tl width/>
            </HList>
            <Text text=current width/>
        </VList>;
    };
};

module Done = {
    let createElement = (
        ~words_per_minute: float,
        ~chars_per_sec: float,
        ~width,
        ~children as _=[],
        ()
    ) => {
        let wpm = Printf.sprintf("WPM: %f", words_per_minute);
        let cps = Printf.sprintf("CPS: %f", chars_per_sec);
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
            words_per_minute=model.stats.words_per_minute
            chars_per_sec=model.stats.chars_per_sec
            typed=typing.typed
            current=typing.current
            target=typing.target
            width
        />
        | Done => <Done
            words_per_minute=model.stats.words_per_minute
            chars_per_sec=model.stats.chars_per_sec
            width
        />
    };
};
