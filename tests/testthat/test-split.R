context("split")

data(starwars, package = "dplyr")
sw_hair <- dplyr::group_by_(starwars, "hair_color")

split_sw_1 <- data.frame(
  target = paste0("slice_starwars_", 1),
  command = paste0("dplyr::slice(starwars, ", 1, ":", 87, ")")
  )
split_sw_2 <- data.frame(
  target = paste0("slice_starwars_", 1:2),
  command = paste0(
    "dplyr::slice(starwars, ",
    seq(1, by = 44, length.out = 2), ":",
    seq(44, by = 44, length.out = 2), ")")
  )
split_sw_12 <- data.frame(
  target = paste0(
    "slice_starwars_",
    stringr::str_pad(1:12, width = 2, pad = "0", side = "left")
    ),
  command = paste0(
    "dplyr::slice(starwars, ",
    seq(1, by = 8, length.out = 12), ":",
    seq(8, by = 8, length.out = 12), ")")
  )

split_swg_1 <- data.frame(
  target = c(
    "slice_sw_hair_grouplist",
    paste0("slice_sw_hair_", 1)
    ),
  command = c(
    "group_list(sw_hair, splits = 1)",
    paste0(
      "sw_hair[dplyr::group_indices(sw_hair) %in% slice_sw_hair_grouplist[[", 1, "]], ]" #nolint: i don't want to split the string
      )
    )
  )
split_swg_2 <- data.frame(
  target = c(
    "slice_sw_hair_grouplist",
    paste0("slice_sw_hair_", 1:2)
    ),
  command = c(
    "group_list(sw_hair, splits = 2)",
    paste0(
      "sw_hair[dplyr::group_indices(sw_hair) %in% slice_sw_hair_grouplist[[", 1:2, "]], ]" #nolint: i don't want to split the string
      )
    )
  )
split_swg_12 <- data.frame(
  target = c(
    "slice_sw_hair_grouplist",
    paste0(
      "slice_sw_hair_",
      stringr::str_pad(1:12, width = 2, pad = "0", side = "left")
      )
    ),
  command = c(
    "group_list(sw_hair, splits = 12)",
    paste0(
      "sw_hair[dplyr::group_indices(sw_hair) %in% slice_sw_hair_grouplist[[", 1:12, "]], ]" #nolint: i don't want to split the string
      )
    )
  )


test_with_dir("drake_split splits correctly", {
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

test_with_dir("drake_split splits correctly - magrittr", {
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

test_with_dir("drake_split splits correctly - dplyr groups", {
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

test_with_dir("drake_split splits correctly - pipe + groups", {
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

test_with_dir("split plans work", {
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
