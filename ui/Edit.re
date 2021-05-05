open Notty
open Pages
open Ui_infix

module Running = {
    let createElement = (
        ~char_per_sec: int,
        ~typed: string,
        ~current: string,
        ~target: list(string),
        ~width,
        ~children as _=[],
        ()
    ) => {
        let time = Printf.sprintf("CPS: %d", char_per_sec);
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
            <Text text=time width pad=(0,0,0,1)/>
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
        ~char_per_sec: int,
        ~width,
        ~children as _=[],
        ()
    ) => {
        let time = Printf.sprintf("CPS: %d", char_per_sec);
        <Text text=time width pad=(0,0,0,1)/>
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
            char_per_sec=model.char_per_sec
            typed=typing.typed
            current=typing.current
            target=typing.target
            width
        />
        | Done => <Done
            char_per_sec=model.char_per_sec
            width
        />
    };
};
