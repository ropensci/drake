context("split")

# I picked the `starwars` dataset becasue It is large enough to actaully split
# nicely. Also, it groups well.
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
  # Notice that i'm attemting at multiple split levels
  for (i in c(1, 4, 7, 12)){
    temp_list <- split_list(sw_hair, splits = i)
    #expect correct number of vectors
    expect_length(temp_list, i)
    lengths_temp <- vapply(temp_list, length, integer(1))
    # expect that the vectors are all there, and nothing else
    expect_identical(sort(unlist(temp_list)), seq(nrow(sw_hair)))
    # expect that elements of a group only show up in one list
    group_lists <- lapply(
      temp_list,
      function(x){
        unique(dplyr::group_indices(sw_hair)[x])
      }
      )
    expect_identical(unlist(group_lists), unique(unlist(group_lists)))
  }
})



test_that("split plans work", {
  dclean()
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
    step_two = dplyr::filter_(..dataset.., "hp_per_cyl > 20"), #nolint: wildcard
    step_three = dplyr::select_(..dataset.., "qsec", "wt"), #nolint: wildcard
    strings_in_dots = "literals"
    )
  # Notice me subsetting the datasets to avoid the list, and only operate on
  # the frames
  plan_analyses <- analyses_split(
    split_plan = plan_split,
    methods = methods,
    wildcard = "..dataset..",
    magic_wildcards = TRUE
    )
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
  dclean()
})
