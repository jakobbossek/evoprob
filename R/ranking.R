#' @title Get algorithm ranking.
#'
#' @description Given a named vector of performance values the function returns
#' the names in sorted order of performance.
#'
#' @param x [\code{numeric}]\cr
#'   Vector of at least two performance values.
#' @param maximize [\code{logical(1)}]\cr
#'   Is the goal to maximize performance values?
#'   Defaults to \code{FALSE}.
#' @return [\code{character}] \code{names(x)} in order of performance.
#' @export
get_algorithm_ranking = function(x, maximize = FALSE) {
  ns = names(x)
  x = order(x, decreasing = maximize)
  return(ns[x])
}
