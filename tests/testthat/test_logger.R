context("logger")

test_that("logger", {
  init.size = 5L
  what = c("P" = "list", "f" = "list", "C" = "character")
  log = init_logger(
    what = what,
    init.size = init.size,
    at = c(1, 3))

  # note that div is not subject to logging since it was not specified in init_logger
  update_logger(log, P = matrix(runif(10), ncol = 2L), C = "a", f = runif(10), iter = 1L, div = letters[1:3])
  # next one is NOT logged (see at parameter)
  update_logger(log, P = matrix(runif(10), ncol = 2L), C = "b", f = runif(10), iter = 2L, div = letters[1:3])
  update_logger(log, P = matrix(runif(10), ncol = 2L), C = "a", f = runif(10), iter = 3L, div = letters[1:3])

  log = log$df
  checkmate::expect_data_frame(log,
    ncols = 4L, nrows = init.size,
    types = c("numeric", unname(what)))
  expect_true(sum(sapply(log$P, is.null)) == 3L)
  expect_true(sum(sapply(log$f, is.null)) == 3L)
})
