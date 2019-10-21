drake_context("dynamic")

test_with_dir("dynamic dependency detection", {
  indices <- seq_len(4)
  plan <- drake_plan(
    u = 4,
    v = seq_len(4),
    w = target(f(v), dynamic = map(v, indices)),
    x = target(w, dynamic = split(w, slices = v)),
    y = target(x, dynamic = cross(x, c(u, y, nope))),
    z = target(nope, dynamic = combine(y, .by = c(w, x, nope)))
  )
  config <- drake_config(plan)
  layout <- config$layout
  expect_equal(layout[["u"]]$deps_dynamic, character(0))
  expect_equal(layout[["v"]]$deps_dynamic, character(0))
  expect_equal(sort(layout[["w"]]$deps_dynamic), sort(c("indices", "v")))
  expect_equal(sort(layout[["x"]]$deps_dynamic), sort(c("v", "w")))
  expect_equal(sort(layout[["y"]]$deps_dynamic), sort(c("u", "x", "y")))
  expect_equal(sort(layout[["z"]]$deps_dynamic), sort(c("w", "x", "y")))
  meta1 <- drake_meta_("u", config)
  meta2 <- drake_meta_("x", config)
  con2 <- drake_config(drake_plan(x = 1))
  meta3 <- drake_meta_("x", con2)
  expect_false(meta1$dynamic)
  expect_true(meta2$dynamic)
  expect_false(meta3$dynamic)
})

test_with_dir("dynamic dependencies in the graph", {
  imported <- 1
  plan <- drake_plan(
    x = 1,
    y = 1,
    z = target(x, dynamic = cross(imported, y))
  )
  config <- drake_config(plan)
  out <- drake_adjacent_vertices(config$graph, v = "z", mode = "in")
  exp <- c("imported", "x", "y")
  expect_equal(sort(out), sort(exp))
})
