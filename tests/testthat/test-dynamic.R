drake_context("dynamic")

test_with_dir("dynamic dependency detection", {
  plan <- drake_plan(
    u = 4,
    v = seq_len(1e6),
    w = target(f(w), dynamic = map(w)),
    x = target(w, dynamic = split(w, slices = v)),
    y = target(x, dynamic = cross(x, c(u, y, nope))),
    z = target(nope, dynamic = combine(y, .by = c(w, x, nope)))
  )
  config <- drake_config(plan)
  layout <- config$layout
  expect_equal(layout[["u"]]$deps_dynamic, character(0))
  expect_equal(layout[["v"]]$deps_dynamic, character(0))
  expect_equal(sort(layout[["x"]]$deps_dynamic), sort(c("v", "w")))
  expect_equal(sort(layout[["y"]]$deps_dynamic), sort(c("u", "x", "y")))
  expect_equal(sort(layout[["z"]]$deps_dynamic), sort(c("w", "x", "y")))
})

test_with_dir("dynamic dependencies in the graph", {
  plan <- drake_plan(
    x = 1,
    y = 1,
    z = target(x, dynamic = map(y))
  )
  config <- drake_config(plan)
  out <- drake_adjacent_vertices(config$graph, v = "z", mode = "in")
  exp <- c("x", "y")
  expect_equal(sort(out), sort(exp))
})
