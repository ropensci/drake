context("split")

# I picked the `starwars` dataset becasue It is large enough to actaully split nicely. Also, it groups well.
data(starwars, package = "dplyr")
sw_hair <- dplyr::group_by_(starwars, "hair_color")

test_that("split_list works - ungrouped", {
  # Notice that i'm attemting at multiple split levels
  for (i in c(1, 4, 7, 12)){
    temp_list <- split_list(starwars, splits = i)
    #expect correct number of vectors
    expect_length(temp_list, i)
    lengths_temp <- vapply(temp_list, length, integer(1))
    expect_true(all(lengths_temp >= floor(nrow(starwars) / i)))
    expect_true(all(lengths_temp <= ceiling(nrow(starwars) / i)))
    # Check that the vectors get monotonically shorter
    expect_true(all(diff(lengths_temp) <= 0))
    # expect that the vectors are all there, and are in order
    expect_identical(unlist(temp_list), seq(nrow(starwars)))
  }
})

test_that("split_list works - grouped", {

})

test_that("drake_split splits correctly", {
  # the default number of groups is nrow() / 1 million
  expect_identical(
    drake_split(starwars),
    split_sw_1
    )
  expect_identical(
    drake_split(starwars, splits = 2),
    split_sw_2
    )
  expect_identical(
    drake_split(starwars, splits = 12),
    split_sw_12
    )
})

test_that("drake_split splits correctly - magrittr", {
  # the default number of groups is nrow() / 1 million
  expect_identical(
    starwars %>% drake_split(),
    split_sw_1
    )
  expect_identical(
    starwars %>% drake_split(splits = 2),
    split_sw_2
    )
  expect_identical(
    starwars %>% drake_split(splits = 12),
    split_sw_12
    )
})

test_that("drake_split splits correctly - dplyr groups", {
  # the default number of groups is nrow() / 1 million
  expect_identical(
    drake_split(sw_hair),
    split_swg_1
    )
  expect_identical(
    drake_split(sw_hair, splits = 2),
    split_swg_2
    )
  expect_identical(
    drake_split(sw_hair, splits = 12),
    split_swg_12
    )
})

test_that("drake_split splits correctly - pipe + groups", {
  # the default number of groups is nrow() / 1 million
  expect_identical(
    sw_hair %>% drake_split(),
    split_swg_1
    )
  expect_identical(
    sw_hair %>% drake_split(splits = 2),
    split_swg_2
    )
  expect_identical(
    sw_hair %>% drake_split(splits = 12),
    split_swg_12
    )
})

test_that("split plans work", {
  # Please not this is mostly an example, and should  not be thought of as an
  # actual, practical workflow
  plan_split <- drake_split(mtcars, splits = 3)
  methods <- plan(
    step_one = dplyr::mutate_(..dataset.., hp_per_cyl = "hp / cyl"), #nolint: wildcard
    # notice the arguement that I'm passing instead of `..dataset`:
    # step_one_..dataset..
    # when analyses does its replacement magic, that will turn into the
    # step_one_mtcars_slice_XX that was generated as a target in the previous
    # step.
    # Further Notce that the filter_ command would not work on the generic
    # ..dataset.., since that doesn't have a hp_per_cyl column.
    step_two = dplyr::filter_(step_one_..dataset.., "hp_per_cyl > 20"), #nolint: wildcard
    step_three = dplyr::select_(step_two_..dataset.., "qsec", "wt"), #nolint: wildcard
    strings_in_dots = "literals"
    )
  plan_analyses <- analyses(plan = methods, datasets = plan_split)
  plan_unsplit <- drake_unsplit(plan_analyses, out_target = "mtc_cleaned")
  plan_lm <- plan(lm_out  = lm(wt ~ qsec, data = mtc_cleaned))
  my_plan <- rbind(plan_split, plan_analyses, plan_unsplit, plan_lm)
  make(my_plan, verbose = FALSE)
  # the normal (nonparallel) pipechain alternative
  sane_way_lm <- mtcars %>%
    dplyr::mutate_(hp_per_cyl = "hp / cyl") %>%
    dplyr::filter_("hp_per_cyl > 20") %>%
    dplyr::select_("qsec", "wt") %>%
    lm("wt ~ qsec", data = .)

  expect_identical(coef(sane_way_lm), coef(lm_out))
})
