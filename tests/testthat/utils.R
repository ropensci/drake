nobuild = function(args){
  built = status()$target
  targets = args$plan$target
  both = intersect(built, targets)
  expect_equal(both, character(0))
}

justbuilt = function(args){
  setdiff(status()$target, imported())
}
