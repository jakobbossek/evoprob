entropy = function(ps) {
  -sum(ps * log(ps, base = 2L))
}

setup = function(fun, ...) {
  args = list(...)
  function(x, ...) {
    do.call(fun, c(list(x), re::insert(args, list(...))))
  }
}
