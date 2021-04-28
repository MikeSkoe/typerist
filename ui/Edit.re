open Notty
open Pages
open Ui_infix

let createElement = (
    ~model: Edit.model,
    ~width,
    ~children as _=[],
    ()
) => {
    let time = Printf.sprintf("time: %d", model.time);
    let typed = String.concat(" ", model.typed);
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
            ((typed === "")
                ? I.empty
                : <Text
                    text=typed
                    width
                    style=A.(fg(gray(10)))
                    pad=(0,1,0,0)
                />)
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