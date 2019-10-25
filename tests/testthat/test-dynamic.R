drake_context("dynamic")

test_with_dir("dynamic dependency detection", {
  indices <- seq_len(4)
  f <- identity
  plan <- drake_plan(
    u = 4,
    v = seq_len(4),
    w = target(f(v), dynamic = map(v, indices)),
    x = target(w, dynamic = split(w, .by = v)),
    y = target(x, dynamic = cross(x, c(u, y, nope))),
    z = target(w, dynamic = combine(y, .by = c(w, x, nope)))
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

test_with_dir("dynamic sub-target indices", {
  expect_equal(subtarget_name("x", seq_len(2)), c("x_1", "x_2"))
  f <- identity
  z_by <- rep(letters[seq_len(4)], each = 4)
  plan <- drake_plan(
    r = seq_len(9),
    s = rep(seq_len(3), 3),
    t = 4,
    u = seq_len(t),
    v = letters[u],
    w = target(f(v), dynamic = map(u, v)),
    x = target(f(r), dynamic = split(r, .by = s)),
    x2 = target(f(r), dynamic = split(r)),
    y = target(seq_len(prod(length(u), length(v))), dynamic = cross(u, v)),
    z = target({z_by; f(y)}, dynamic = combine(y, .by = z_by)), # nolint
    z2 = target(f(y), dynamic = combine(y))
  )
  make(plan[, c("target", "command")])
  config <- drake_config(plan)
  for (i in seq_len(2)) {
    expect_equal(number_subtargets("w", config), 4L)
    expect_equal(number_subtargets("x", config), 3L)
    expect_equal(number_subtargets("x2", config), 1L)
    expect_equal(number_subtargets("y", config), 16L)
    expect_equal(number_subtargets("z", config), 4L)
    expect_equal(number_subtargets("z2", config), 1L)
  }
  for (i in seq_len(4)) {
    ew <- list(u = i, v = i)
    expect_equal(subtarget_deps("w", i, config), ew)
  }
  for (i in seq_len(4)) {
    for (j in seq_len(4)) {
      ey <- list(u = i, v = j)
      k <- 4 * (i - 1) + j
      expect_equal(subtarget_deps("y", k, config), ey)
    }
  }
  ew <- list(r = c(0L, 3L, 6L))
  for (i in seq_len(3)) {
    ew$r <- ew$r + 1L
    expect_equal(subtarget_deps("x", i, config), ew)
  }
  for (i in seq_len(4)) {
    ez <- list(y = seq(from = 4 * (i - 1) + 1, 4 * i))
    expect_equal(subtarget_deps("z", i, config), ez)
  }
})

test_with_dir("dynamic subvalues", {
  expect_equal(dynamic_subvalue(letters, 2), "b")
  expect_equal(dynamic_subvalue(letters, c(2, 4)), c("b", "d"))
  m <- mtcars
  expect_equal(dynamic_subvalue(m, 4), m[4,, drop = FALSE]) # nolint
  expect_equal(dynamic_subvalue(m, c(4, 5)), m[c(4, 5),, drop = FALSE]) # nolint
  m <- as.matrix(m)
  expect_equivalent(dynamic_subvalue(m, 4), m[4,, drop = FALSE]) # nolint
  expect_equivalent(dynamic_subvalue(m, c(4, 5)), m[c(4, 5),, drop = FALSE]) # nolint
  m <- array(seq_len(prod(seq(2, 6))), dim = seq(2, 6)) # nolint
  expect_equivalent(dynamic_subvalue(m, 1), m[1,,,,]) # nolint
  expect_equivalent(dynamic_subvalue(m, c(1, 2)), m[c(1, 2),,,,]) # nolint
})

test_with_dir("dynamic map", {
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(y + 1, dynamic = map(y))
  )
  make(plan)
  expect_equal(readd(x), seq_len(4))
  expect_equal(readd(y_1), 2)
  expect_equal(readd(y_2), 3)
  expect_equal(readd(y_3), 4)
  expect_equal(readd(y_4), 5)
  expect_equal(readd(z_1), 3)
  expect_equal(readd(z_2), 4)
  expect_equal(readd(z_3), 5)
  expect_equal(readd(z_4), 6)
})

test_with_dir("dynamic cross", {
  plan <- drake_plan(
    x1 = letters[seq_len(2)],
    x2 = LETTERS[seq_len(2)],
    y1 = target(paste0(x1, x2), dynamic = map(x1, x2)),
    y2 = target(paste0(x2, x1), dynamic = map(x1, x2)),
    z1 = target(paste0(x1, x2), dynamic = cross(x1, x2)),
    z2 = target(paste0(x1, y2), dynamic = cross(x1, y2)),
    z3 = target(paste0(y1, x2), dynamic = cross(y1, x2)),
    z4 = target(paste0(y1, y2), dynamic = cross(y1, y2)),
  )
  make(plan)
  expect_equal(readd(x1), letters[seq_len(2)])
  expect_equal(readd(x2), LETTERS[seq_len(2)])
  expect_equal(readd(y1_1), "aA")
  expect_equal(readd(y1_2), "bB")
  expect_equal(readd(y2_1), "Aa")
  expect_equal(readd(y2_2), "Bb")
  expect_equal(readd(z1_1), "aA")
  expect_equal(readd(z1_2), "aB")
  expect_equal(readd(z1_3), "bA")
  expect_equal(readd(z1_4), "bB")
  expect_equal(readd(z2_1), "aAa")
  expect_equal(readd(z2_2), "aBb")
  expect_equal(readd(z2_3), "bAa")
  expect_equal(readd(z2_4), "bBb")
  expect_equal(readd(z3_1), "aAA")
  expect_equal(readd(z3_2), "aAB")
  expect_equal(readd(z3_3), "bBA")
  expect_equal(readd(z3_4), "bBB")
  expect_equal(readd(z4_1), "aAAa")
  expect_equal(readd(z4_2), "aABb")
  expect_equal(readd(z4_3), "bBAa")
  expect_equal(readd(z4_4), "bBBb")
})
