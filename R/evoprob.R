#' @title Instance-evolving EA.
#'
#' @description To be written ...
#'
#' @param n [\code{integer(1L)}]\cr
#'   Desired instance size.
#' @param P [\code{list}]\cr
#'   Initial population.
#'   This is problem dependent. Therefore, the used must provide it.
#' @param runner.fun [\code{function}]\cr
#'   Function used to run algorithms on problem instance.
#'   Should return aggregated performance values.
#' @param fitness.fun [\code{function}]\cr
#'   Fitness function.
#' @param mut.fun [\code{function}]\cr
#'   Mutation operator.
#' @param diversity.fun [\code{function}]\cr
#'   Diversity function, i.e. function that calculates the diversity of a set
#'   of instances.
#' @param max.iters [\code{integer(1)}]\cr
#'   Stopping condition: maximum number of iterations.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{list}]
#' @export
evoprob = function(
  n,
  P,
  runner.fun,
  fitness.fun,
  mut.fun,
  diversity.fun = NULL,
  max.iters = 10L,
  ...
  ) {

  # sanity checks
  n = checkmate::asInt(n, lower = 5L)
  checkmate::assert_list(P, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_function(runner.fun)
  checkmate::assert_function(fitness.fun)
  checkmate::assert_function(mut.fun)
  checkmate::assert_function(diversity.fun, null.ok = TRUE)
  max.iters = checkmate::asInt(max.iters, lower = 1L)

  # vars
  iter = 0L
  mu = length(P)
  st = Sys.time() # start time

  # run algos and calculate fitness
  runres = lapply(P, runner.fun, ...)
  if (is.null(names(runres[[1L]])))
    re::stopf("[evoprob::evoprob] runner.res should return named vector of performance values.")
  fP = unname(lapply(runres, fitness.fun, ...))

  # monitoring
  re::catf("[evoprob] Initialization done\n")

  # do EA magic
  while (iter < max.iters) {
    # start time iteration
    sti = Sys.time()

    # sample random individual
    x = P[[sample(mu, size = 1L)]]

    # generate exactly one offspring
    y = mut.fun(x, ...)
    runresy = runner.fun(y, ...)
    fy = unname(fitness.fun(runresy, ...))

    # now check if we have (1+1)-EA or (mu+1)-EA
    if (mu == 1L) {
      # simply replace population if mutant is better
      # print(fy)
      # print(fP[[1L]])
      # print("===")
      if (is_better(fy, fP[[1L]])) {
        fP[[1L]] = fy
        P[[1L]] = y
        runres = runresy
      }
    } else {
      # now comes the diversity fun
      re::stopf("[evoprob::evoprob] (mu+1)-EA not yet implemented :(")
    }

    # monitoring
    iter = iter + 1L
    tpi = as.numeric(difftime(Sys.time(), sti, units = "secs"))
    tp = as.numeric(difftime(Sys.time(), st, units = "secs"))
    #FIXME: this works for (1+1) and scalar-fitness function only
    re::catf("[evoprob] Iter %i (%.2f / %.2f), best-fitness: %.4f\n", iter, tpi, tp, fP[[1L]])
  }

  return(list(
    P = P,
    fP = fP,
    runres = runres
  ))
}

is_better = function(x, y) {
  n = length(x)
  if (n == 1L)
    return(x >= y)
  return(is_lexicographically_better(x, y))
}

is_lexicographically_better = function(x, y) {
  # NOTE: we maximize all objectives
  n = length(x)
  for (i in seq_len(n)) {
    if (x[i] > y[i])
      return(TRUE)
    else if (x[i] < y[i])
      return(FALSE)
  }
  return(FALSE)
}
