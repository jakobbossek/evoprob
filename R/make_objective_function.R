# GOAL: have function f with single parameter x that runs
# each A_i in A repls times with pars_i on x and returns the
# (aggregated) results.

#' @title Factory for objective functions.
#'
#' @description Given a set of \eqn{n} algorithms \eqn{A} (parameter \code{A}) with
#' parameters \eqn{\theta_i, i \in A} (parameter \code{A.pars}) the goal is
#' to produce a function \code{f = function(x, ...)} that expects a problem
#' instance \code{x} and optional arguments \code{...}. The function shall
#' run each algorithm \eqn{A_i \in A} parameterized with \eqn{\theta_i} on
#' \code{x}. In case of stochastic/randomized algorithms each algorithm should
#' be run multiple times (parameter \code{repls}) returning an aggregated
#' value (parameter \code{aggr.fun}).
#'
#' @param A [\code{character}]\cr
#'   Character vector of at least two algorithm names.
#'   Note that the names must be valid arguments for the \dQuote{algorithm}
#'   parameter of parameter \code{runner.fun}.
#' @param A.pars [\code{list of lists}]\cr
#'   A named list (names equal to \code{A}) where each sub-list should contain
#'   parameters for the respective algorithm.
#' @param runner.fun [\code{function(x, algorithm, ...)}]\cr
#'   A function that expects an instance \code{x}, an algorithm name \code{algorithm}
#'   (see parameter \code{A}) and optional further parameters (see parameter
#'   \code{A.pars}).
#' @param repls [\code{integer}]\cr
#'   How many independent runs should be conducted for each algorithm on an
#'   instance? Defaults to 1, but should be adjusted if the corresponding algorithm(s)
#'   is/are of stochastic nature.
#'   If a single value is passed it is used for all algorithms.
#' @param aggr.fun [\code{function(x, ...)}]\cr
#'   Function used to aggregate results of multiple runs.
#'   Only applied if \code{repls[i]} is greater than one for algorithm \code{A[i]}.
#'   Defaults to \code{\link[base]{mean}}.
#' @param named.result [\code{logical(1)}]\cr
#'   Should the result vector be named with \code{A}?
#'   Default is \code{TRUE}.
#' @return [\code{function(x, ...)}] Factorized function (see description).
#' @export
make_objective_function = function(A, A.pars, runner.fun, repls = 1L, aggr.fun = base::mean, named.result = TRUE) {
  checkmate::assert_character(A, min.len = 2L, any.missing = FALSE, all.missing = FALSE)
  n = length(A)
  checkmate::assert_list(A.pars, len = n, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_function(runner.fun, args = c("x", "algorithm"), ordered = TRUE)
  checkmate::assert_integerish(repls, lower = 1L, min.len = 1L, max.len = n, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_function(aggr.fun)
  checkmate::assert_flag(named.result)

  # make vars available in returned function
  force(A)
  force(A.pars)
  force(runner.fun)
  force(repls)
  force(aggr.fun)
  force(named.result)

  # if there is only a single repls value passed use it for all
  # algorithms
  if (length(repls) == 1L)
    repls = rep(repls, n)

  # build the function
  function(x, ...) {
    # FIXME: allow multicore parallelization, i.e. build grid and
    # parallelMap::.../future::... on it.
    # for each algorithm
    res = sapply(seq_len(n), function(i) {
      # for each independent run
      runres = sapply(seq_len(repls[i]), function(r) {
        runner.args = c(list(x, algorithm = A[i]), A.pars[[i]])
        do.call(runner.fun, runner.args)
      })
      # aggregate if algorithm is stochastic
      if (repls[i] > 1L)
        runres = aggr.fun(runres)
      return(runres)
    })
    if (named.result)
      names(res) = A
    return(res)
  }
}
