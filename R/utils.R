entropy = function(ps) {
  -sum(ps * log(ps, base = 2L))
}
