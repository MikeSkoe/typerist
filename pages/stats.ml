type t = {
      chars_per_sec: float;
      words_per_minute: float;
}

let empty = {
      chars_per_sec = 0.;
      words_per_minute = 0.;
}

let make chars_per_sec words_per_minute = {
      chars_per_sec = chars_per_sec;
      words_per_minute = words_per_minute;
}

let get_cps str seconds =
      let seconds = float_of_int seconds in
      let str_len = str
            |> String.length
            |> float_of_int
      in
      str_len /. seconds

let get_wpm str seconds =
      let seconds = float_of_int seconds in
      let word_count =
            str
            |> String.split_on_char ' '
            |> List.length
            |> float_of_int
      in
      word_count /. seconds *. 60.

let update strs seconds =
      let str = String.(concat " " strs) in
      let cps = get_cps str seconds in
      let wpm = get_wpm str seconds in
      {
            chars_per_sec = cps;
            words_per_minute = wpm;
      }