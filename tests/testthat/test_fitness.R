context("fitness")

test_that("'norder' function produce correct output", {
  perfs = c("A" = 10, "B" = 30, "C" = 40)
  expected = (40 - 30) * (30 - 10) # =200
  testthat::expect_equal(fitness_diverse_noorder(perfs, maximize = FALSE), expected)
  testthat::expect_equal(fitness_diverse_noorder(perfs, maximize = TRUE), expected)
})

test_that("'explicit' function produces correct output", {
  perfs = c("A" = 10, "B" = 30, "C" = 40)
  ranking = c("A", "B", "C")

  # maximize
  expected = c(2, 0, 30)
  testthat::expect_equal(fitness_diverse_explicit(perfs, ranking, maximize = FALSE), expected)

  # minimize
  #FIXME: method does not work with maximize=TRUE correctly so far
  # expected = c(0, -30, -Inf)
  # testthat::expect_equal(fitness_diverse_explicit(perfs, ranking, maximize = TRUE), expected)
})
