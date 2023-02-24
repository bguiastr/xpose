# errors are returned for bad filename input

    Code
      xpose_save(plot = plot)
    Condition
      [1m[33mError[39m:[22m
      [33m![39m Argument `file` required.

---

    Code
      xpose_save(plot = plot, file = paths_1[1])
    Condition
      [1m[33mError[39m in `ggsave()`:[22m
      [38;5;250m[33m![38;5;250m Unknown graphics device "abcd"[39m

---

    Code
      xpose_save(plot = plot, file = paths_1[3])
    Condition
      [1m[33mError[39m in `ggsave()`:[22m
      [38;5;250m[33m![38;5;250m Unknown graphics device ""[39m

