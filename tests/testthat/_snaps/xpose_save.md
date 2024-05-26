# errors are returned for bad filename input

    Code
      xpose_save(plot = plot)
    Condition
      Error:
      ! Argument `file` required.

---

    Code
      xpose_save(plot = plot, file = paths_1[1])
    Condition
      Error in `ggsave()`:
      ! Unknown graphics device "abcd"

