drake_context("dynamic")

test_with_dir("undefined grouping variables become dynamic", {
  expect_silent(
    out <- drake_plan(
      w = seq_len(1e6),
      x = target(f(w), dynamic = map(w)),
      y = target(x, dynamic = cross(x)),
      z = target(y, dynamic = combine(y))
    )
  )
})

