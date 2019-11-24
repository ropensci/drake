drake_context("dynamic")

test_with_dir("dynamic dependency detection", {
  indices <- seq_len(4)
  f <- identity
  plan <- drake_plan(
    v = 4,
    w = seq_len(4),
    x = target(f(v), dynamic = map(v, indices)),
    y = target(x, dynamic = cross(x, c(v, y, nope))),
    z = target(w, dynamic = group(x, y, .by = c(w, nope)))
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
    z = target({z_by; f(y)}, dynamic = group(y, .by = z_by)), # nolint
    z2 = target(f(y), dynamic = group(y))
  )
  make(plan[, c("target", "command")])
  config <- drake_config(plan)
  config$ht_dynamic <- ht_new()
  config$ht_dynamic_size <- ht_new()
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

test_with_dir("empty dynamic transform", {
  plan <- drake_plan(x = target("x", dynamic = map()))
  expect_error(make(plan), regexp = "no admissible grouping variables")
})

test_with_dir("invalidating a subtarget invalidates the parent", {
  plan <- drake_plan(
    x = seq_len(2),
    y = target(x, dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  clean(list = subtargets(y)[1])
  expect_equal(outdated(config), "y")
  make(plan)
  exp <- sort(c("y", subtargets(y)[1]))
  expect_equal(sort(c(justbuilt(config))), exp)
})

test_with_dir("lots of maps", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  suppressWarnings(rm(x1, x2, x3, x4, x5, envir = envir))
  plan <- drake_plan(
    x1 = seq_len(4),
    x2 = target(x1, dynamic = map(x1)),
    x3 = target(x2, dynamic = map(x2)),
    x4 = target(x3, dynamic = map(x3)),
    x5 = target(x4, dynamic = map(x4))
  )
  make(plan)
  expect_equal(unlist(readd(x5)), seq_len(4))
})

test_with_dir("dynamic map flow", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  caching <- scenario$caching
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(y + 1, dynamic = map(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
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
  # change a static dep
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(y + 2, dynamic = map(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  config <- drake_config(plan)
  expect_true(length(justbuilt(config)) > 0L)
  expect_true(all(grepl("^z", justbuilt(config))))
  ys <- subtargets(y)
  zs <- subtargets(z)
  expect_equal(readd(ys[1], character_only = TRUE), 2)
  expect_equal(readd(ys[2], character_only = TRUE), 3)
  expect_equal(readd(ys[3], character_only = TRUE), 4)
  expect_equal(readd(ys[4], character_only = TRUE), 5)
  expect_equal(readd(zs[1], character_only = TRUE), 4)
  expect_equal(readd(zs[2], character_only = TRUE), 5)
  expect_equal(readd(zs[3], character_only = TRUE), 6)
  expect_equal(readd(zs[4], character_only = TRUE), 7)
  # change nothing
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(justbuilt(config), character(0))
  ys <- subtargets(y)
  zs <- subtargets(z)
  expect_equal(readd(ys[1], character_only = TRUE), 2)
  expect_equal(readd(ys[2], character_only = TRUE), 3)
  expect_equal(readd(ys[3], character_only = TRUE), 4)
  expect_equal(readd(ys[4], character_only = TRUE), 5)
  expect_equal(readd(zs[1], character_only = TRUE), 4)
  expect_equal(readd(zs[2], character_only = TRUE), 5)
  expect_equal(readd(zs[3], character_only = TRUE), 6)
  expect_equal(readd(zs[4], character_only = TRUE), 7)
  # change part of a dynamic dep
  plan <- drake_plan(
    x = as.integer(c(1, 5, 3, 6)),
    y = target(x + 1, dynamic = map(x)),
    z = target(y + 2, dynamic = map(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- justbuilt(config)
  exp <- c("x", "y", subtargets(y)[c(2, 4)], "z", subtargets(z)[c(2, 4)])
  expect_equal(sort(out), sort(exp))
  ys <- subtargets(y)
  zs <- subtargets(z)
  expect_equal(readd(ys[1], character_only = TRUE), 2)
  expect_equal(readd(ys[2], character_only = TRUE), 6)
  expect_equal(readd(ys[3], character_only = TRUE), 4)
  expect_equal(readd(ys[4], character_only = TRUE), 7)
  expect_equal(readd(zs[1], character_only = TRUE), 4)
  expect_equal(readd(zs[2], character_only = TRUE), 8)
  expect_equal(readd(zs[3], character_only = TRUE), 6)
  expect_equal(readd(zs[4], character_only = TRUE), 9)
  # change nothing
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(justbuilt(config), character(0))
  expect_equal(readd(ys[1], character_only = TRUE), 2)
  expect_equal(readd(ys[2], character_only = TRUE), 6)
  expect_equal(readd(ys[3], character_only = TRUE), 4)
  expect_equal(readd(ys[4], character_only = TRUE), 7)
  expect_equal(readd(zs[1], character_only = TRUE), 4)
  expect_equal(readd(zs[2], character_only = TRUE), 8)
  expect_equal(readd(zs[3], character_only = TRUE), 6)
  expect_equal(readd(zs[4], character_only = TRUE), 9)
  # insert a new dynamic dep
  plan <- drake_plan(
    x = as.integer(c(1, 5, 200, 3, 6)),
    y = target(x + 1, dynamic = map(x)),
    z = target(y + 2, dynamic = map(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- justbuilt(config)
  exp <- c("x", "y", subtargets(y)[3], "z", subtargets(z)[3])
  expect_equal(sort(out), sort(exp))
  ys <- subtargets(y)
  zs <- subtargets(z)
  expect_equal(readd(ys[1], character_only = TRUE), 2)
  expect_equal(readd(ys[2], character_only = TRUE), 6)
  expect_equal(readd(ys[3], character_only = TRUE), 201)
  expect_equal(readd(ys[4], character_only = TRUE), 4)
  expect_equal(readd(ys[5], character_only = TRUE), 7)
  expect_equal(readd(zs[1], character_only = TRUE), 4)
  expect_equal(readd(zs[2], character_only = TRUE), 8)
  expect_equal(readd(zs[3], character_only = TRUE), 203)
  expect_equal(readd(zs[4], character_only = TRUE), 6)
  expect_equal(readd(zs[5], character_only = TRUE), 9)
  # change dynamic and static dep
  plan <- drake_plan(
    x = as.integer(c(7, 8, 9)),
    y = target(x ^ 2, dynamic = map(x)),
    z = target(y ^ 2, dynamic = map(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- justbuilt(config)
  exp <- c("x", "y", subtargets(y), "z", subtargets(z))
  expect_equal(sort(out), sort(exp))
  ys <- subtargets(y)
  zs <- subtargets(z)
  expect_equal(readd(ys[1], character_only = TRUE), 49)
  expect_equal(readd(ys[2], character_only = TRUE), 64)
  expect_equal(readd(ys[3], character_only = TRUE), 81)
  expect_equal(readd(zs[1], character_only = TRUE), 2401)
  expect_equal(readd(zs[2], character_only = TRUE), 4096)
  expect_equal(readd(zs[3], character_only = TRUE), 6561)
})

test_with_dir("change 2 sub-deps (sub-target filtering)", {
  plan <- drake_plan(
    x = c("a", "b", "c", "d"),
    y = target(x, dynamic = map(x))
  )
  make(plan)
  expect_equal(unlist(readd(y)), c("a", "b", "c", "d"))
  plan <- drake_plan(
    x = c("a", "X", "Y", "d"),
    y = target(x, dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  expect_equal(unlist(readd(y)), c("a", "X", "Y", "d"))
  out <- justbuilt(config)
  exp <- c("x", "y", subtargets(y)[c(2, 3)])
  expect_equal(sort(out), sort(exp))
})

test_with_dir("identical sub-dep hashes", {
  plan <- drake_plan(
    x = rep("x", 4),
    y = target(sample.int(n = 1e9, size = 1), dynamic = map(x))
  )
  make(plan)
  out4 <- unlist(readd(y))
  expect_equal(length(out4), 4)
  expect_equal(length(unique(out4)), 4)
  plan <- drake_plan(
    x = rep("x", 5),
    y = target(sample.int(n = 1e9, size = 1), dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  out5 <- unlist(readd(y))
  expect_equal(length(out5), 5)
  expect_equal(length(unique(out5)), 5)
  expect_equal(out5[-5], out4)
  out <- justbuilt(config)
  exp <- c("x", "y", subtargets(y)[5])
  expect_equal(sort(out), sort(exp))
  plan <- drake_plan(
    x = rep("x", 3),
    y = target(sample.int(n = 1e9, size = 1), dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  out3 <- unlist(readd(y))
  expect_equal(length(out3), 3)
  expect_equal(length(unique(out3)), 3)
  expect_equal(out4[-4], out3)
  out <- justbuilt(config)
  exp <- c("x", "y")
  expect_equal(sort(out), sort(exp))
})

test_with_dir("dynamic cross values", {
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

test_with_dir("dynamic cross flow", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  caching <- scenario$caching
  assert_vals <- function(vals) {
    subs <- c(subtargets(y), subtargets(z))
    expect_equal(length(subs), length(vals))
    for (i in seq_along(subs)) {
      expect_equal(readd(subs[i], character_only = TRUE), vals[[i]])
    }
  }
  plan <- drake_plan(
    v = "-",
    w = letters[seq_len(2)],
    x = LETTERS[seq_len(3)],
    y = target(paste0(w, x), dynamic = cross(w, x)),
    z = target(paste0(v, y), dynamic = cross(v, y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(readd(w), letters[seq_len(2)])
  expect_equal(readd(x), LETTERS[seq_len(3)])
  vals <- c("aA", "aB", "aC", "bA", "bB", "bC")
  vals <- c(vals, paste0("-", vals))
  assert_vals(vals)
  # change a static dep
  plan <- drake_plan(
    v = "+",
    w = letters[seq_len(2)],
    x = LETTERS[seq_len(3)],
    y = target(paste0(w, x), dynamic = cross(w, x)),
    z = target(paste0(v, y), dynamic = cross(v, y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  config <- drake_config(plan)
  expect_true(length(justbuilt(config)) > 0L)
  expect_equal(sort(justbuilt(config)), sort(c("v", "z", subtargets(z))))
  vals <- gsub("-", "+", vals, fixed = TRUE)
  assert_vals(vals)
  # change nothing
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(justbuilt(config), character(0))
  assert_vals(vals)
  # change part of a dynamic dep
  plan <- drake_plan(
    v = "+",
    w = letters[seq_len(2)],
    x = LETTERS[c(1, 2, 4)],
    y = target(paste0(w, x), dynamic = cross(w, x)),
    z = target(paste0(v, y), dynamic = cross(v, y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- justbuilt(config)
  exp <- c("x", "y", "z", subtargets(y)[c(3, 6)], subtargets(z)[c(3, 6)])
  expect_equal(sort(out), sort(exp))
  vals <- gsub("C", "D", vals, fixed = TRUE)
  assert_vals(vals)
  # change nothing
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(justbuilt(config), character(0))
  assert_vals(vals)
  # remove a dynamic dep
  plan <- drake_plan(
    v = "+",
    w = letters[seq_len(2)],
    x = LETTERS[c(2, 4)],
    y = target(paste0(w, x), dynamic = cross(w, x)),
    z = target(paste0(v, y), dynamic = cross(v, y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(sort(justbuilt(config)), sort(c("x", "y", "z")))
  vals <- vals[!grepl("A$", vals)]
  assert_vals(vals)
  # insert a new dynamic dep
  plan <- drake_plan(
    v = "+",
    w = letters[c(1, 5, 2)],
    x = LETTERS[c(2, 4)],
    y = target(paste0(w, x), dynamic = cross(w, x)),
    z = target(paste0(v, y), dynamic = cross(v, y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  exp <- c("w", "y", "z", subtargets(y)[c(3, 4)], subtargets(z)[c(3, 4)])
  expect_equal(sort(justbuilt(config)), sort(exp))
  vals <- c("aB", "aD", "eB", "eD", "bB", "bD")
  vals <- c(vals, paste0("+", vals))
  assert_vals(vals)
  # change dynamic and static dep
  plan <- drake_plan(
    v = "=",
    w = letters[c(1, 5, 2)],
    x = LETTERS[c(2, 4)],
    y = target(paste(w, x), dynamic = cross(w, x)),
    z = target(paste0(v, y), dynamic = cross(v, y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- justbuilt(config)
  exp <- c("v", "y", subtargets(y), "z", subtargets(z))
  expect_equal(sort(out), sort(exp))
  vals <- c("a B", "a D", "e B", "e D", "b B", "b D")
  vals <- c(vals, paste0("=", vals))
  assert_vals(vals)
})

test_with_dir("switch the order of cross sub-targets", {
  plan <- drake_plan(
    x = LETTERS[seq_len(2)],
    y = letters[seq_len(2)],
    z = target(c(x, y), dynamic = cross(x, y))
  )
  make(plan)
  plan <- drake_plan(
    x = LETTERS[seq_len(2)],
    y = letters[seq_len(2)],
    z = target(c(x, y), dynamic = cross(y, x))
  )
  make(plan)
  config <- drake_config(plan)
  expect_equal(justbuilt(config), "z")
})

test_with_dir("dynamic group flow without by", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  caching <- scenario$caching
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- readd(subtargets(z), character_only = TRUE)
  exp <- c(2, 3, 4, 5)
  expect_equal(out, exp)
  # change nothing
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  config <- drake_config(plan)
  expect_equal(justbuilt(config), character(0))
  # Change static dep
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x + 2, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- readd(subtargets(z), character_only = TRUE)
  expect_equal(out, c(3, 4, 5, 6))
  exp <- c("y", "z", subtargets(y), subtargets(z))
  expect_equal(sort(justbuilt(config)), sort(exp))
  # change nothing
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(justbuilt(config), character(0))
  expect_equal(out, c(3, 4, 5, 6))
  # Insert dynamic dep
  plan <- drake_plan(
    x = c(1, 2, 10, 4),
    y = target(x + 2, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- readd(subtargets(z), character_only = TRUE)
  exp <- c(3, 4, 12, 6)
  expect_equal(out, exp)
  exp <- c("x", "y", "z", subtargets(y), subtargets(z))
  expect_equal(sort(justbuilt(config)), sort(exp))
})

test_with_dir("dynamic group with by", {
  plan <- drake_plan(
    u = seq_len(4),
    v = seq_len(4) + 1,
    w = c("b", "b", "b", "a"),
    x = target(u, dynamic = map(u)),
    y = target(v, dynamic = map(v)),
    z = target(
      list(x = do.call("c", x), y = do.call("c", y), my_by = w),
      dynamic = group(x, y, .by = w)
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
      list(x = sum(do.call("c", x)), y = do.call("c", y)),
      dynamic = group(x, y, .by = w)
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

test_with_dir("insert .by piece by piece", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  caching <- scenario$caching
  plan <- drake_plan(
    w = c("a", "b", "c", "c"),
    x = seq_len(4),
    y = target(x + 1, dynamic = map(x)),
    z = target(
      list(y = unlist(y), w = w),
      dynamic = group(y, .by = w)
    )
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- readd(z)
  exp <- list(
    list(y = 2L, w = "a"),
    list(y = 3L, w = "b"),
    list(y = c(4L, 5L), w = "c")
  )
  expect_equal(out, exp)
})

test_with_dir("dynamic group flow with by", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  suppressWarnings(rm(
    a, b, c, f, g, h, i, j, my_function, my_plan,
    random_rows, reg1, reg2, simulate,
    envir = envir
  ))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  caching <- scenario$caching
  plan <- drake_plan(
    w = c("a", "b", "c", "c"),
    x = seq_len(4),
    y = target(x + 101, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y, .by = w))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  out <- lapply(subtargets(z), readd, character_only = TRUE)
  exp <- list(102, 103, c(104, 105))
  expect_equal(out, exp)
  # change nothing
  make(plan)
  config <- drake_config(plan)
  expect_equal(justbuilt(config), character(0))
  # change static dep
  plan <- drake_plan(
    w = c("a", "b", "c", "c"),
    x = seq_len(4),
    y = target(x + 2, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y, .by = w))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  config <- drake_config(plan)
  exp <- c("y", "z", subtargets(y), subtargets(z))
  expect_equal(sort(justbuilt(config)), sort(exp))
  out <- lapply(subtargets(z), readd, character_only = TRUE)
  exp <- list(3, 4, c(5, 6))
  expect_equal(out, exp)
  # change dynamic sub-dep
  plan <- drake_plan(
    w = c("a", "b", "c", "c"),
    x = as.integer(c(1, 2, 3, 10)),
    y = target(x + 2, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y, .by = w))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  exp <- c("x", "y", "z", subtargets(y)[4], subtargets(z)[3])
  expect_equal(sort(justbuilt(config)), sort(exp))
  out <- lapply(subtargets(z), readd, character_only = TRUE)
  exp <- list(3, 4, c(5, 12))
  expect_equal(out, exp)
  # change nothing
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  config <- drake_config(plan)
  expect_equal(justbuilt(config), character(0))
  # insert dynamic sub-dep
  plan <- drake_plan(
    w = c("a", "a", "b", "c", "c"),
    x = as.integer(c(1, 0, 2, 3, 10)),
    y = target(x + 2, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y, .by = w))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  exp <- c("w", "x", "y", "z", subtargets(y)[2], subtargets(z)[1])
  expect_equal(sort(justbuilt(config)), sort(exp))
  out <- lapply(subtargets(z), readd, character_only = TRUE)
  exp <- list(c(3, 2), 4, c(5, 12))
  expect_equal(out, exp)
  # change some groupings
  plan <- drake_plan(
    w = c("a", "b", "b", "c", "c"),
    x = as.integer(c(1, 0, 2, 3, 10)),
    y = target(x + 2, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y, .by = w))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  # One of the z sub-targets has the same dynamic sub-dependency hash
  # as a previous build.
  exp <- c("w", "z", subtargets(z)[2])
  expect_equal(sort(justbuilt(config)), sort(exp))
  out <- lapply(subtargets(z), readd, character_only = TRUE)
  exp <- list(3, c(2, 4), c(5, 12))
  expect_equal(out, exp)
  # change .by but not groupings
  plan <- drake_plan(
    w = c("X", "Y", "Y", "Z", "Z"),
    x = as.integer(c(1, 0, 2, 3, 10)),
    y = target(x + 2, dynamic = map(x)),
    z = target(unlist(y), dynamic = group(y, .by = w))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching
  )
  expect_equal(sort(justbuilt(config)), c("w", "z"))
  out <- lapply(subtargets(z), readd, character_only = TRUE)
  exp <- list(3, c(2, 4), c(5, 12))
  expect_equal(out, exp)
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
  expect_true(all(exp %in% cached()))
  expect_equal(length(intersect(sub1, sub2)), 3L)
})

test_with_dir("dynamic map over unequal vars", {
  plan <- drake_plan(
    x = seq_len(4),
    y = seq_len(5),
    z = seq_len(5),
    w = target(x + y + z, dynamic = map(x, y, z))
  )
  expect_error(make(plan), "all grouping variables")
})

test_with_dir("dynamic group over unequal vars", {
  plan <- drake_plan(
    x = seq_len(4),
    y = seq_len(5),
    x2 = target(x, dynamic = map(x)),
    y2 = target(y, dynamic = map(y)),
    z = target(c(x2, y2), dynamic = group(x2, y2))
  )
  expect_error(make(plan), "all grouping variables")
})

test_with_dir("dynamic group over unequal vars", {
  plan <- drake_plan(
    x = seq_len(4),
    y = seq_len(5),
    x2 = target(x, dynamic = map(x)),
    y2 = target(y, dynamic = map(y)),
    z = target(x2, dynamic = group(x2, .by = y))
  )
  expect_error(make(plan), "all grouping variables")
})

test_with_dir("group: static targets and .by precedence", {
  plan <- drake_plan(
    x = c("a", "a", "b", "b"),
    y = target(x, dynamic = group(x))
  )
  make(plan)
  expect_equal(readd(y), list(c("a", "a", "b", "b")))
  plan <- drake_plan(
    x = c("a", "a", "b", "b"),
    y = seq_len(4),
    z = target(y, dynamic = group(y, .by = x))
  )
  make(plan)
  expect_equal(readd(z), list(c(1, 2), c(3, 4)))
  plan <- drake_plan(
    x = c("a", "a", "b", "b"),
    y = as.character(seq_len(4)),
    z = target(c(x, y), dynamic = group(y, .by = x))
  )
  make(plan)
  expect_equal(readd(z), list(c("a", "1", "2"), c("b", "3", "4")))
  plan <- drake_plan(
    x = c("a", "a", "b", "b"),
    y = target(x, dynamic = group(x, .by = x))
  )
  make(plan)
  expect_equal(readd(y), list(c("a", "a"), c("b", "b")))
})

test_with_dir("group multiple targets", {
  plan <- drake_plan(
    w = c("a", "a", "b", "b"),
    x = c(1, 2, 3, 4),
    y = c(11, 12, 13, 14),
    z = target(list(x, y), dynamic = group(x, y, .by = w))
  )
  make(plan)
  out <- readd(z)
  exp <- list(list(c(1, 2), c(11, 12)), list(c(3, 4), c(13, 14)))
  expect_equal(out, exp)
})

test_with_dir("formats applied to subtargets but not their parents", {
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x, format = "rds", dynamic = map(x))
  )
  make(plan)
  cache <- drake_cache()
  for (i in seq_len(4)) {
    y <- subtargets(y)[i]
    expect_equal(readd(y, character_only = TRUE), i)
    expect_equal(cache$get_value(cache$get_hash(y)), i)
    ref <- cache$storr$get(y)
    expect_true(inherits(ref, "drake_format_rds"))
    expect_equal(length(ref), 1L)
    expect_true(nchar(ref) < 100)
    expect_false(is.numeric(ref))
  }
  ref <- cache$storr$get("y")
  expect_false(inherits(ref, "drake_format_rds"))
  special <- file.path(".drake", "drake", "return")
  expect_true(file.exists(special))
  special_files <- list.files(special)
  expect_equal(length(special_files), 4L)
})

test_with_dir("non-rds formats and dynamic branching (#1059)", {
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = data.frame(x = seq_len(2), y = seq_len(2)),
    y = target(x, dynamic = map(x), format = "fst")
  )
  make(plan, session_info = FALSE)
  exp <- readd(x)
  out <- do.call(rbind, readd(y))
  expect_equal(out, exp)
  cache <- drake_cache()
  ref <- cache$storr$get("y")
  expect_false(inherits(ref, "drake_format_fst"))
  for (i in seq_len(2)) {
    y <- subtargets(y)[i]
    ref <- cache$storr$get(y)
    expect_true(inherits(ref, "drake_format_fst"))
    expect_false(is.data.frame(ref))
  }
})

test_with_dir("runtime predictions for dynamic targets", {
  skip_on_cran()
  skip_if_not_installed("lubridate")
  f <- function(x) {
    x
  }
  plan <- drake_plan(
    x = f(letters[seq_len(4)]),
    y = target(x, dynamic = map(x)),
    z = target(y, dynamic = map(y))
  )
  config <- drake_config(plan)
  suppressWarnings(predict_runtime(config))
  make(plan)
  predict_runtime(config)
  known_times <- c(0, 0, rep(1, 9))
  names(known_times) <- c("y", "z", "x", subtargets(y), subtargets(z))
  time1 <- as.integer(predict_runtime(config, known_times = known_times))
  time2 <- as.integer(
    predict_runtime(config, known_times = known_times, jobs = 2)
  )
  time4 <- as.integer(
    predict_runtime(config, known_times = known_times, jobs = 4)
  )
  expect_equal(time1, 8L)
  expect_equal(time2, 4L)
  expect_equal(time4, 2L)
})

test_with_dir("dynamic subtargets and RNGs", {
  plan <- drake_plan(
    x = seq_len(4),
    y = target(rnorm(1), dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  out <- vapply(
    subtargets(y),
    readd,
    character_only = TRUE,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )
  expect_equal(length(unique(out)), 4L)
  clean(destroy = TRUE)
  make(plan)
  out2 <- vapply(
    subtargets(y),
    readd,
    character_only = TRUE,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )
  expect_equal(out, out2)
  plan <- drake_plan(
    x = seq_len(4),
    y = target(rnorm(1), dynamic = map(x), seed = 1234)
  )
  make(plan)
  out3 <- vapply(
    subtargets(y),
    readd,
    character_only = TRUE,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )
  expect_false(any(out2 == out3))
  plan <- drake_plan(
    x = seq_len(4),
    y = target(rnorm(1), dynamic = map(x))
  )
  make(plan, seed = 1234)
  out4 <- vapply(
    subtargets(y),
    readd,
    character_only = TRUE,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )
  expect_false(any(out2 == out4))
  expect_false(any(out3 == out4))
})

test_with_dir("dynamic condition triggers are not allowed", {
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x, trigger = trigger(condition = x > 2), dynamic = map(x))
  )
  expect_error(drake_config(plan), regexp = "forbidden")
})

test_with_dir("dynamic change triggers are not allowed", {
  plan <- drake_plan(
    x = seq_len(4),
    y = target(x, trigger = trigger(change = x), dynamic = map(x))
  )
  expect_error(drake_config(plan), regexp = "forbidden")
})

test_with_dir("dynamic parent recovery", {
  skip_on_cran()
  plan <- drake_plan(
    x = letters[seq_len(4)],
    y = target(file.create(x), dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  files <- letters[seq_len(4)]
  expect_true(all(file.exists(files)))
  unlink(files)
  expect_false(any(file.exists(files)))
  clean(list = c("y", subtargets(y)))
  make(plan, recover = TRUE)
  expect_false(any(file.exists(files)))
  expect_equal(outdated(config), character(0))
  make(plan)
  expect_equal(justbuilt(config), character(0))
})

test_with_dir("subtarget recovery", {
  plan <- drake_plan(
    x = letters[seq_len(2)],
    y = target(file.create(x), dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  clean(list = subtargets(y)[1])
  unlink(readd(x))
  expect_equal(outdated(config), "y")
  make(plan, recover = TRUE)
  expect_equal(outdated(config), character(0))
  expect_false(any(file.exists(c("a", "b"))))
  expect_equal(sort(justbuilt(config)), sort(c(subtargets(y)[1])))
})

test_with_dir("failed dynamic data recovery", {
  skip_on_cran()
  plan <- drake_plan(
    x = letters[seq_len(4)],
    y = target(file.create(x), dynamic = map(x))
  )
  config <- drake_config(plan)
  make(plan)
  files <- letters[seq_len(4)]
  expect_true(all(file.exists(files)))
  unlink(files)
  expect_false(any(file.exists(files)))
  config$cache$del(subtargets(y)[1])
  config$cache$del(subtargets(y)[1], namespace = "recover")
  make(plan, recover = TRUE)
  expect_true(file.exists("a"))
})

test_with_dir("row-wise dynamic map()", {
  skip_on_cran()
  plan <- drake_plan(
    x = mtcars,
    y = target(x, dynamic = map(x))
  )
  make(plan)
  ys <- lapply(subtargets(y), readd, character_only = TRUE)
  for (y in ys) {
    expect_true(is.data.frame(y))
    expect_equal(nrow(y), 1)
  }
  df <- do.call("rbind", ys)
  expect_equal(df, mtcars)
})

test_with_dir("dynamic loadd() and readd()", {
  plan <- drake_plan(
    x = mtcars[seq_len(4), ],
    y = target(x, dynamic = map(x))
  )
  make(plan)
  out <- readd(x)
  expect_equal(out, mtcars[seq_len(4), ])
  out <- readd(y)
  expect_true(is.list(out))
  expect_false(is.data.frame(out))
  expect_equal(do.call("rbind", out), mtcars[seq_len(4), ])
  skip_if_not_installed("bindr")
  for (lazy in c("eager", "promise", "bind")) {
    loadd(y, lazy = lazy)
    expect_true(is.list(y))
    expect_false(is.data.frame(y))
    expect_equal(do.call("rbind", y), mtcars[seq_len(4), ])
    rm(y)
  }
  out <- readd(y, subtargets = c(2, 4))
  expect_true(is.list(out))
  expect_false(is.data.frame(out))
  expect_equal(length(out), 2)
  expect_equal(do.call("rbind", out), mtcars[c(2, 4), ])
  for (lazy in c("eager", "promise", "bind")) {
    loadd(y, lazy = lazy, subtargets = c(2, 4))
    expect_equal(length(y), 2)
    expect_true(is.list(y))
    expect_false(is.data.frame(y))
    expect_equal(do.call("rbind", y), mtcars[c(2, 4), ])
    rm(y)
  }
})

test_with_dir("dynamic hpc", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  skip_if_not_installed("future")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  future::plan(future::multicore, workers = 2L)
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  suppressWarnings(rm(
    a, b, c, f, g, h, i, j, my_function, my_plan,
    random_rows, reg1, reg2, simulate,
    envir = envir
  ))
  hpc_tests <- function(envir, parallelism, caching) {
    plan <- drake_plan(
      low = c("a", "b"),
      high = target(toupper(low), dynamic = map(low)),
      combos = target(paste0(low, high), dynamic = cross(low, high)),
      index = target(substr(combos, 0L, 1L), dynamic = map(combos)),
      groups = target(
        unlist(combos),
        dynamic = group(combos, .by = index)
      )
    )
    make(
      plan,
      envir = envir,
      jobs = 2L,
      parallelism = parallelism,
      caching = caching
    )
    out <- readd(groups)
    exp <- list(c("aA", "aB"), c("bA", "bB"))
    expect_equal(out, exp)
    config <- drake_config(plan)
    expect_equal(outdated(config), character(0))
    make(
      plan,
      envir = envir,
      jobs = 2L,
      parallelism = parallelism,
      caching = caching
    )
    expect_equal(justbuilt(config), character(0))
    plan <- drake_plan(
      low = c("a", "b"),
      high = target(toupper(low), dynamic = map(low)),
      combos = target(paste0(low, high), dynamic = cross(low, high)),
      index = target(substr(combos, 0L, 1L), dynamic = map(combos)),
      groups = target(
        paste0(unlist(combos), "+"),
        dynamic = group(combos, .by = index)
      )
    )
    make(
      plan,
      envir = envir,
      jobs = 2L,
      parallelism = parallelism,
      caching = caching
    )
    out <- justbuilt(config)
    exp <- c("groups", subtargets(groups))
    expect_equal(sort(out), sort(exp))
    out <- readd(groups)
    exp <- list(c("aA+", "aB+"), c("bA+", "bB+"))
    expect_equal(out, exp)
    clean(destroy = TRUE)
  }
  for (parallelism in c("clustermq", "future")) {
    for (caching in c("master", "worker")) {
      hpc_tests(envir, parallelism, caching)
    }
  }
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("dynamic max_expand", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  suppressWarnings(rm(dyn1, dyn2, dyn3, dyn4, dyn5, envir = envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  caching <- scenario$caching
  plan <- drake_plan(
    dyn1 = seq_len(10),
    dyn2 = seq_len(10),
    dyn3 = target(dyn1, dynamic = map(dyn1)),
    dyn4 = target(c(dyn1, dyn2), dynamic = cross(dyn1, dyn2)),
    dyn5 = target(unlist(dyn1), dynamic = group(dyn1, .by = dyn2))
  )
  make(
    plan,
    envir = envir,
    parallelism = parallelism,
    jobs = jobs,
    caching = caching,
    max_expand = 2
  )
  expect_equal(readd(dyn1), seq_len(10))
  expect_equal(readd(dyn2), seq_len(10))
  expect_equal(readd(dyn3), list(1L, 2L))
  expect_equal(readd(dyn4), list(c(1L, 1L), c(1L, 2L)))
  expect_equal(readd(dyn5), list(1L, 2L))
  config <- drake_config(plan)
  make(plan, max_expand = 4)
  out <- justbuilt(config)
  exp <- c(
    "dyn3",
    "dyn4",
    "dyn5",
    subtargets(dyn3)[c(3, 4)],
    subtargets(dyn4)[c(3, 4)],
    subtargets(dyn5)[c(3, 4)]
  )
  expect_equal(sort(out), sort(exp))
  expect_equal(readd(dyn3), list(1L, 2L, 3L, 4L))
  expect_equal(readd(dyn4), list(c(1L, 1L), c(1L, 2L), c(1L, 3L), c(1L, 4L)))
  expect_equal(readd(dyn5), list(1L, 2L, 3L, 4L))
  make(plan, max_expand = 3)
  expect_equal(sort(justbuilt(config)), sort(c("dyn3", "dyn4", "dyn5")))
  expect_equal(sort(out), sort(exp))
  expect_equal(readd(dyn3), list(1L, 2L, 3L))
  expect_equal(readd(dyn4), list(c(1L, 1L), c(1L, 2L), c(1L, 3L)))
  expect_equal(readd(dyn5), list(1L, 2L, 3L))
  suppressWarnings(rm(dyn1, dyn2, dyn3, dyn4, dyn5, envir = envir))
})

test_with_dir("bad cross trace (#1052)", {
  plan <- drake_plan(
    a = letters[seq_len(2)],
    b = seq_len(2),
    c = target(paste(a, b), dynamic = cross(a, b, .trace = c(a, b, x)))
  )
  expect_error(drake_config(plan), regexp = "illegal dynamic trace variables")
})

test_with_dir("bad group trace (#1052)", {
  plan <- drake_plan(
    a = letters[seq_len(2)],
    b = seq_len(2),
    c = target(paste(a, b), dynamic = cross(a, b, .trace = c(a, b))),
    a_crossed = get_trace(c, "a"),
    d = target(unlist(c), dynamic = group(c, .by = a_crossed, .trace = a))
  )
  expect_error(
    drake_config(plan),
    regexp = "the only legal dynamic trace variable"
  )
})

test_with_dir("dynamic map trace (#1052)", {
  plan <- drake_plan(
    a = letters[seq_len(4)],
    b = target(a, dynamic = map(a, .trace = a)),
    c = target(b, dynamic = map(a, b, .trace = c(a, b)))
  )
  make(plan)
  value <- drake_cache()$get("c")
  expect_equal(get_trace("a", value), readd(a))
  expect_equal(read_trace("a", "c"), readd(a))
  exp <- as.character(drake_cache()$get("b"))
  expect_equal(read_trace("b", "c"), exp)
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), character(0))
  plan <- drake_plan(
    a = letters[seq_len(4)],
    b = target(a, dynamic = map(a, .trace = a)),
    # Changing the trace should only trigger the parent target.
    c = target(b, dynamic = map(a, b, .trace = a))
  )
  make(plan)
  expect_equal(justbuilt(config), "c")
})

test_with_dir("dynamic cross trace (#1052)", {
  expect_error(read_trace("a", "b"))
  plan <- drake_plan(
    w = LETTERS[seq_len(3)],
    x = letters[seq_len(2)],
    y = target(x, dynamic = map(x)),
    z = target(c(w, x, y), dynamic = cross(w, x, y, .trace = c(x, w)))
  )
  make(plan)
  value <- drake_cache()$get("z")
  out <- get_trace("w", value)
  exp <- rep(LETTERS[seq_len(3)], each = 4)
  expect_equal(out, exp)
  out <- read_trace("w", "z")
  expect_equal(out, exp)
  out <- get_trace("x", value)
  exp <- rep(letters[c(1, 1, 2, 2)], times = 3)
  expect_equal(out, exp)
  out <- read_trace("x", "z")
  expect_equal(out, exp)
})

test_with_dir("dynamic group trace (#1052)", {
  plan <- drake_plan(
    w = LETTERS[seq_len(3)],
    x = letters[seq_len(2)],
    y = target(c(w, x), dynamic = cross(w, x, .trace = w)),
    w_tr = get_trace("w", y),
    z = target(y, dynamic = group(y, .by = w_tr, .trace = w_tr))
  )
  make(plan)
  value <- drake_cache()$get("z")
  expect_equal(get_trace("w_tr", value), LETTERS[seq_len(3)])
  expect_equal(read_trace("w_tr", "z"), LETTERS[seq_len(3)])
})


test_with_dir("dynamic combine() does not exist", {
  skip_on_cran()
  plan <- drake_plan(
    x = seq_len(8),
    y = target(x, dynamic = combine(x, .by = x))
  )
  expect_error(make(plan), regexp = "does not exist")
})

test_with_dir("trace responds to dynamic max_expand (#1073)", {
  skip_on_cran()
  x <- rep(seq_len(10), 2)
  plan <- drake_plan(
    y = target(
      x,
      dynamic = map(x, .trace = x)
    ),
    y_trace = get_trace("x", y),
    z = target(
      sum(unlist(y)),
      dynamic = group(y, .by = y_trace)
    )
  )
  make(plan, max_expand = 2)
  expect_equal(read_trace("x", "y"), seq_len(2))
  expect_equal(readd(z), list(1, 2))
})

test_with_dir("data frame trace (#1074)", {
  skip_on_cran()
  x <- data.frame(
    a = rep(seq_len(10), 2),
    b = letters[seq_len(20)],
    stringsAsFactors = FALSE
  )
  plan <- drake_plan(
    y = target(
      x,
      dynamic = map(x, .trace = x)
    )
  )
  make(plan, max_expand = 3)
  expect_equal(readd(y), unname(split(x[seq_len(3), ], f = seq_len(3))))
  expect_equal(read_trace("x", "y"), x[seq_len(3), ])
})

test_with_dir("dynamic branching and memory strategies", {
  skip_on_cran()
  plan <- drake_plan(
    v = "-",
    w = letters[seq_len(2)],
    x = LETTERS[seq_len(3)],
    y = target(paste0(w, x), dynamic = cross(w, x)),
    z = target(paste0(v, y), dynamic = cross(v, y))
  )
  # Should clear config$envir_subtargets without a fuss.
  make(plan, memory_strategy = "autoclean")
  expect_true(is.list(readd(z)))
})
