#' @title No-order fitness function.
#'
#' @description Given performance values \eqn{p_1, \ldots, p_n} of \eqn{n \geq 3}
#' algorithms, this function first sorts the values in increasing order resulting
#' in the order statistics \eqn{p_{(1)}, \ldots, p_{(n)}}, i.e. \eqn{p_{(i)}} is
#' the \eqn{i}th largest value. Next the function calculates the scalar fitness
#' value implementing the following formula:
#' \eqn{
#' f(p_1, \ldots, p_n) = \sum_{i=2}^{n-1} \left(p_{(i+1)} - p_{(i)}\right) \cdot \left(p_{(i)} - p_{(i-1)}\right).
#' }
#'
#' @param x [\code{numeric}]\cr
#'   Vector of at least two performance values.
#' @param maximize [\code{logical(1)}]\cr
#'   Is the goal to maximize performance values?
#'   Defaults to \code{FALSE}.
#' @return [\code{numeric(1)}]
#' @export
fitness_diverse_noorder = function(x, maximize = FALSE) {
  checkmate::assert_numeric(x, min.len = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_flag(maximize)
  #FIXME: do we need to distinguish max/min?
  # we could sort in increasing order and use the absolute values of
  # the differences in the factors.
  x = sort(x, decreasing = maximize)
  n = length(x)
  f = 0
  if (n == 2L)
    return(x[2L] - x[1L])
  # NOTE: R-loop not critical since n is < 6 for all reasonable setups
  for (i in 2:seq_len(n - 1L)) {
    f = f + ((x[i + 1L] - x[i]) * (x[i] - x[i - 1L]))
  }
  return(f)
}

#' @title Explicit order fitness function.
#'
#' @description To be written ...
#'
#' @param x [\code{numeric}]\cr
#'   Named vector of at least two performance values.
#' @param ranking [\code{character}]\cr
#'   Desired, i.e. explicit, ranking.
#' @param maximize [\code{logical(1)}]\cr
#'   Is the goal to maximize performance values?
#'   Defaults to \code{FALSE}.
#' @return [\code{numeric(3)}]
#' @export
fitness_diverse_explicit = function(x, ranking, maximize = FALSE) {
  #FIXME: do we need maximize? (se fitness_diverse_noorder)
  #FIXME: implementation is ugly -> refactor
  checkmate::assert_numeric(x, min.len = 2L, any.missing = FALSE, all.missing = FALSE, names = "unique")
  checkmate::assert_set_equal(names(x), ranking)
  checkmate::assert_flag(maximize)

  actual_ranking = get_algorithm_ranking(x, maximize)
  # get good directions
  good = list()
  bad = list()
  n = length(x)
  for (i in seq_len(n - 1L)) {
    pi1 = ranking[i]
    pi2 = ranking[i + 1L]
    if (x[pi1] <= x[pi2]) {
      good = c(good, list(c(pi1, pi2)))
    } else {
      bad = c(bad, list(c(pi1, pi2)))
    }
  }
  ngood = length(good)
  fbad = -Inf
  if (length(fbad) > 0) {
    fbad = sum(sapply(bad, function(p) {
      abs(x[p[1L]] - x[p[2L]])
    }))
  }
  fgood = -Inf
  if (ngood > 0) {
    fgood = sum(sapply(good, function(p) {
      abs(x[p[1L]] - x[p[2L]])
    }))
  }
  return(c(ngood, fbad, fgood))
}
