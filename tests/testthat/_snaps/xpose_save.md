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
      [1m[22m[33m![39m Unknown graphics device "abcd"

---

    Code
      xpose_save(plot = plot, file = paths_1[3])
    Condition
      [1m[33mError[39m in `ggsave()`:[22m
      [1m[22m[33m![39m Unknown graphics device ""

---

    Code
      xpose_save(plot = plot, file = basename(paths_1))
    Condition
      [1m[33mWarning[39m:[22m
      `filename` must have length 1, not length 3.
      ! Only the first, ']8;;file:///Users/bguiastr/Documents/Pharmacometrics/r_stuffs/_projects/xpose/produced/package/xpose/tests/testthat/test_plot.abcdtest_plot.abcd]8;;', will be used.
      [1m[33mError[39m in `ggsave()`:[22m
      [1m[22m[33m![39m Unknown graphics device "abcd"

