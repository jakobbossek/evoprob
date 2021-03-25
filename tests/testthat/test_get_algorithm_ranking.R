context("get_algorithm_ranking")

test_that("get_algorithm_ranking", {
  A = c("A", "B", "C")
  perfs = c(10, 100, 1000)
  names(perfs) = A
  testthat::expect_equal(get_algorithm_ranking(perfs, maximize = FALSE), A)
  testthat::expect_equal(get_algorithm_ranking(perfs, maximize = TRUE), rev(A))
})
