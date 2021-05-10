type msg =
      | ToEdit
      | ToMenu
      | SaveResult of Stats.t
      | Exit
