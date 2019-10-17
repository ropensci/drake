drake_context("dynamic")

if (FALSE) {

test_with_dir("undefined grouping variables become dynamic", {
  expect_silent(
    out <- drake_plan(
      w = seq_len(1e6),
      x = target(f(w), transform = map(w)),
      y = target(x, transform = cross(x)),
      z = target(y, transform = combine(y))
    )
  )
})

}
