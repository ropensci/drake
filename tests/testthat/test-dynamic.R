drake_context("dynamic")

test_with_dir("dynamic dependency detection", {
  indices <- seq_len(4)
  f <- identity
  plan <- drake_plan(
    v = 4,
    w = seq_len(4),
    x = target(f(v), dynamic = map(v, indices)),
    y = target(x, dynamic = cross(x, c(v, y, nope))),
    z = target(w, dynamic = combine(x, y, .by = c(w, nope)))
  )
  config <- drake_config(plan)
  layout <- config$layout
  expect_equal(layout[["v"]]$deps_dynamic, character(0))
  expect_equal(layout[["w"]]$deps_dynamic, character(0))
  expect_equal(sort(layout[["x"]]$deps_dynamic), sort(c("indices", "v")))
  expect_equal(sort(layout[["y"]]$deps_dynamic), sort(c("v", "x", "y")))
  expect_equal(sort(layout[["z"]]$deps_dynamic), sort(c("w", "x", "y")))
  meta1 <- drake_meta_("v", config)
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
  f <- identity
  z_by <- rep(letters[seq_len(4)], each = 4)
  plan <- drake_plan(
    r = seq_len(9),
    s = rep(seq_len(3), 3),
    t = 4,
    u = seq_len(t),
    v = letters[u],
    w = target(f(v), dynamic = map(u, v)),
    y = target(seq_len(prod(length(u), length(v))), dynamic = cross(u, v)),
    z = target({z_by; f(y)}, dynamic = combine(y, .by = z_by)), # nolint
    z2 = target(f(y), dynamic = combine(y))
  )
  make(plan[, c("target", "command")])
  config <- drake_config(plan)
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
  ys <- subtargets(y)
  zs <- subtargets(z)
  expect_equal(readd(ys[1], character_only = TRUE), 2)
  expect_equal(readd(ys[2], character_only = TRUE), 3)
  expect_equal(readd(ys[3], character_only = TRUE), 4)
  expect_equal(readd(ys[4], character_only = TRUE), 5)
  expect_equal(readd(zs[1], character_only = TRUE), 3)
  expect_equal(readd(zs[2], character_only = TRUE), 4)
  expect_equal(readd(zs[3], character_only = TRUE), 5)
  expect_equal(readd(zs[4], character_only = TRUE), 6)
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(y + 2, dynamic = map(y))
  )
  make(plan)
  config <- drake_config(plan)
  expect_true(all(grepl("^z", justbuilt(config))))
  expect_equal(readd(ys[1], character_only = TRUE), 2)
  expect_equal(readd(ys[2], character_only = TRUE), 3)
  expect_equal(readd(ys[3], character_only = TRUE), 4)
  expect_equal(readd(ys[4], character_only = TRUE), 5)
  expect_equal(readd(zs[1], character_only = TRUE), 4)
  expect_equal(readd(zs[2], character_only = TRUE), 5)
  expect_equal(readd(zs[3], character_only = TRUE), 6)
  expect_equal(readd(zs[4], character_only = TRUE), 7)
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
    z4 = target(paste0(y1, y2), dynamic = cross(y1, y2))
  )
  make(plan)
  rc <- function(target) {
    readd(target, character_only = TRUE)
  }
  expect_equal(readd(x1), letters[seq_len(2)])
  expect_equal(readd(x2), LETTERS[seq_len(2)])
  y1 <- subtargets(y1)
  y2 <- subtargets(y2)
  z1 <- subtargets(z1)
  z2 <- subtargets(z2)
  z3 <- subtargets(z3)
  z4 <- subtargets(z4)
  expect_equal(rc(y1[1]), "aA")
  expect_equal(rc(y1[2]), "bB")
  expect_equal(rc(y2[1]), "Aa")
  expect_equal(rc(y2[2]), "Bb")
  expect_equal(rc(z1[1]), "aA")
  expect_equal(rc(z1[2]), "aB")
  expect_equal(rc(z1[3]), "bA")
  expect_equal(rc(z1[4]), "bB")
  expect_equal(rc(z2[1]), "aAa")
  expect_equal(rc(z2[2]), "aBb")
  expect_equal(rc(z2[3]), "bAa")
  expect_equal(rc(z2[4]), "bBb")
  expect_equal(rc(z3[1]), "aAA")
  expect_equal(rc(z3[2]), "aAB")
  expect_equal(rc(z3[3]), "bBA")
  expect_equal(rc(z3[4]), "bBB")
  expect_equal(rc(z4[1]), "aAAa")
  expect_equal(rc(z4[2]), "aABb")
  expect_equal(rc(z4[3]), "bBAa")
  expect_equal(rc(z4[4]), "bBBb")
})

test_with_dir("dynamic combine", {
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(c(y), dynamic = combine(y))
  )
  make(plan)
  out <- readd(subtargets(z), character_only = TRUE)
  exp <- list(2, 3, 4, 5)
  expect_equal(out, exp)
})

test_with_dir("dynamic combine with by", {
  plan <- drake_plan(
    u = seq_len(4),
    v = seq_len(4) + 1,
    w = c("b", "b", "b", "a"),
    x = target(u, dynamic = map(u)),
    y = target(v, dynamic = map(v)),
    z = target(
      list(x = do.call(c, x), y = do.call(c, y), my_by = w),
      dynamic = combine(x, y, .by = w)
    )
  )
  make(plan)
  out1 <- readd(subtargets(z)[1], character_only = TRUE)
  out2 <- readd(subtargets(z)[2], character_only = TRUE)
  exp1 <- list(x = seq_len(3), y = seq_len(3) + 1, my_by = "b")
  exp2 <- list(x = 4, y = 5, my_by = "a")
  expect_equal(out1, exp1)
  expect_equal(out2, exp2)
  plan <- drake_plan(
    u = seq_len(4),
    v = seq_len(4) + 1,
    w = c("b", "b", "b", "a"),
    x = target(u, dynamic = map(u)),
    y = target(v, dynamic = map(v)),
    z = target(
      list(x = sum(do.call(c, x)), y = do.call(c, y)),
      dynamic = combine(x, y, .by = w)
    )
  )
  make(plan)
  config <- drake_config(plan)
  expect_true(all(grepl("^z", justbuilt(config))))
  out1 <- readd(subtargets(z)[1], character_only = TRUE)
  out2 <- readd(subtargets(z)[2], character_only = TRUE)
  exp1 <- list(x = 6, y = seq_len(3) + 1)
  exp2 <- list(x = 4, y = 5)
  expect_equal(out1, exp1)
  expect_equal(out2, exp2)
})

test_with_dir("make a dep dynamic later on", {
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(y + 1, dynamic = map(y))
  )
  make(plan[, c("target", "command")])
  make(plan)
  config <- drake_config(plan)
  built <- c("y", "z", subtargets(y), subtargets(z))
  expect_equal(sort(built), sort(justbuilt(config)))
})

test_with_dir("subtarget name invalidation", {
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x))
  )
  make(plan)
  expect_equal(readd(y_0b3474bd), 2)
  sub1 <- subtargets(y)
  exp <- c("x", "y", sub1)
  expect_equal(sort(cached()), sort(exp))
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 2, dynamic = map(x))
  )
  make(plan)
  expect_equal(readd(y_0b3474bd), 3)
  expect_equal(sort(cached()), sort(exp))
  plan <- drake_plan(
    x = as.integer(seq_len(4) + 1),
    y = target(x + 2, dynamic = map(x))
  )
  make(plan)
  sub2 <- subtargets(y)
  exp <- c("x", "y", sub2)
  expect_equal(sort(cached()), sort(exp))
  expect_equal(length(intersect(sub1, sub2)), 3L)
})

