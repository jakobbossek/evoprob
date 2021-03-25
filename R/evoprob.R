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
#' @param log.pop [\code{logical(1)}]\cr
#'   Store the population in each iteration?
#'   Defaults to \code{FALSE}.
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
  log.pop = FALSE,
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
  checkmate::assertFlag(log.pop)

  if (is.null(diversity.fun) && (mu > 1L))
    re::stopf("[evoprob::evoprob] For mu >= 2 diversity.fun must not be NULL.")

  # vars
  iter = 0L
  mu = length(P)
  st = Sys.time() # start time

  # bookkeeping
  # log = init_bookkeeping(n = max.iters, x = P, )

  # run algos and calculate fitness
  runres = lapply(P, runner.fun, ...)

  # Extract algorithm names
  A = names(runres[[1L]])
  Aperms = get_all_permutations(A)
  divtab = rep(0L, length(Aperms))
  names(divtab) = Aperms
  div = NA_real_

  if (is.null(A))
    re::stopf("[evoprob::evoprob] runner.fun must return a *named* vector of performance values.")
  fP = unname(lapply(runres, fitness.fun, ...))

  # monitoring
  re::catf("[evoprob] Initialization done\n")

  if (!is.null(diversity.fun) && (mu > 1L)) {
    # FIXME: get_algorithm_ranking should have option to returns collapsed string
    # FIXME: outsource in helper function init_diversity_table(...)
    actual_order = sapply(lapply(runres, get_algorithm_ranking), re::collapse, sep = "-")
    for (r in actual_order) {
      divtab[r] = divtab[r] + 1L
    }
    div = diversity.fun(unname(divtab) / mu)
  }
  divtabinit = divtab

  # do EA magic
  while (iter < max.iters) {
    # start time iteration
    sti = Sys.time()

    # sample random individual
    idx.parent = sample(mu, size = 1L)
    x = P[[idx.parent]]

    # generate exactly one offspring
    y = mut.fun(x, ...)
    runresy = runner.fun(y, ...)
    actual_order_y = re::collapse(get_algorithm_ranking(runresy), sep = "-")
    fy = unname(fitness.fun(runresy, ...))

    # now check if we have (1+1)-EA or (mu+1)-EA
    if (mu == 1L) {
      # simply replace population if mutant is better
      if (is_better(fy, fP[[1L]])) {
        fP[[1L]] = fy
        P[[1L]] = y
        runres = runresy
      }
    } else {
      # now comes the diversity fun
      #FIXME: outsource into update_diversity
      divs = sapply(seq_len(mu), function(i) {
        divtab2 = divtab
        divtab2[actual_order[i]] = divtab2[actual_order[i]] - 1L
        divtab2[actual_order_y] = divtab2[actual_order_y] + 1L
        entropy(unname(divtab2) / mu)
      })
      max.div.idx = which.max(divs)
      if (divs[max.div.idx] > div) {
        # if entropy gets bigger by replacing some instance with y, do it
        # and finish iteration
        catf("Iter: %i, Improved diversity from %.4f to %.4f replacing %i-th individual.\n", iter, div, divs[max.div.idx], max.div.idx)
        div = divs[max.div.idx]
        divtab[actual_order[max.div.idx]] = divtab[actual_order[max.div.idx]] - 1L
        divtab[actual_order_y] = divtab[actual_order_y] + 1L
        actual_order[max.div.idx] = actual_order_y
        P[[max.div.idx]] = y
        fP[[max.div.idx]] = fy
        runres[[max.div.idx]] = runresy
      } else {
        # otherwise, i.e. entropy cannot increase, replace instance which leads to
        idx.same.order = which(actual_order == actual_order_y)
        #catf("Same order: %i\n", length(idx.same.order))
        #FIXME: remove element with the same count in divtab?
        if (length(idx.same.order) == 0) {
          re::catf("No elements with same order.\n")
          next
        }
        #FIXME: here, fP[[i]] needs to be scalar, i.e. for noorder fitness
        idx.same.order.min.f = which.min(as.numeric(fP)[idx.same.order])
        #print(as.numeric(fP)[idx.same.order])
        idx.replace = idx.same.order[idx.same.order.min.f]
        #catf("Same order min fitness: %i\n", idx.replace)
        if (is_better(fy, fP[[idx.replace]])) {
          catf("Iter: %i, Cannot improve diversity. Replacing %i (f=%.4f) with mutant (f=%.4f).\n", iter, idx.replace, fP[[idx.replace]], fy)
          P[[idx.replace]] = y
          fP[[idx.replace]] = fy
          runres[[idx.replace]] = runresy
        } else {
          catf("Iter: %i, Cannot improve neither diversity nor fitness.\n", iter, idx.replace, fP[[idx.replace]], fy)
        }
      }


      #re::stopf("[evoprob::evoprob] (mu+1)-EA not yet implemented :(")
    }

    #Sys.sleep(1)
    # monitoring
    iter = iter + 1L
    tpi = as.numeric(difftime(Sys.time(), sti, units = "secs"))
    tp = as.numeric(difftime(Sys.time(), st, units = "secs"))
    #FIXME: this works for (1+1) and scalar-fitness function only
    re::catf("[evoprob] Iter %i (%.2f / %.2f), diversity: %.4f, best-fitness: %.4f\n", iter, tpi, tp, div, fP[[1L]])
  }

  return(list(
    P = P,
    fP = fP,
    runres = runres,
    divtab = divtab,
    runres = runres,
    divtabinit = divtabinit
  ))
}

# init_bookkeeping = function() {
#   env = new.env()
#   env$log =
# }


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
