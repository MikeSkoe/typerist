open Notty
open Pages
open Ui_infix

let createElement = (
    ~model: Edit.model,
    ~width,
    ~children as _=[],
    ()
) => {
    let time = Printf.sprintf("CPS: %d", model.char_per_sec);
    let (target_hd, target_tl) = switch (model.target) {
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
                text=model.typed
                width
                style=A.(fg(gray(10)))
                pad=(model.typed == "" ? (0,0,0,0) : (0,1,0,0))
            />
            <Text
                text=target_hd
                width
                style=A.(fg(black) <+> bg(white))
                pad=(0,1,0,0)
            />
            <Text text=target_tl width/>
        </HList>
        <Text text=model.current width/>
    </VList>;
}
