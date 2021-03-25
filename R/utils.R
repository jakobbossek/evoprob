entropy = function(ps) {
  s = ps * log(ps, base = 2L)
  nans = is.nan(s)
  if (any(nans))
    s[nans] = 0
  -sum(s)
}

setup = function(fun, ...) {
  args = list(...)
  function(x, ...) {
    do.call(fun, c(list(x), re::insert(args, list(...))))
  }
}

get_all_permutations = function(x) {
  n = length(x)
  l = replicate(n, list(x))
  l = c(l, list(stringsAsFactors = FALSE))

  all = do.call(expand.grid, l)
  perms = all[apply(all, 1L, function(x) {length(unique(x)) == n}), ]

  perms = unname(apply(perms, 1L, re::collapse, sep = "-"))
  return(perms)
}
