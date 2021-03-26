#' @title Get algorithm ranking.
#'
#' @description Given a named vector of performance values the function returns
#' the names in sorted order of performance.
#'
#' @param x [\code{numeric} | \code{list of numeric vectors}]\cr
#'   Named vector of at least two performance values or list of such.
#' @param maximize [\code{logical(1)}]\cr
#'   Is the goal to maximize performance values?
#'   Defaults to \code{FALSE}.
#' @param as.string [\code{logical(1L)}]\cr
#'   Convert to string representation, i.e. \code{c("A", "B", "C")} to
#'   \code{"A---B---C"}.
#' @param sep [\code{character(1)}]\cr
#'   Separator used to \dQuote{glue together} if \code{as.string = TRUE}.
#'   Default is \dQuote{---}.
#' @return [\code{character}] \code{names(x)} in order of performance.
#' @export
get_algorithm_ranking = function(x, maximize = FALSE, as.string = FALSE, sep = "---") {
  rank.fun = function(x, maximize, as.string) {
    ns = names(x)
    x = order(x, decreasing = maximize)
    res = ns[x]
    if (as.string)
      res = re::collapse(res, sep = sep)
    return(res)
  }
  if (is.list(x)) {
    res = lapply(x, rank.fun, maximize, as.string)
    if (as.string)
      return(unlist(res))
    else
      return(res)
  }
  rank.fun(x, maximize, as.string)
}
