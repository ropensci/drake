# library(testthat); library(devtools); load_all()
context("conserve-memory")
source("utils.R")


# This is a sketch of a test I might do when 
# I want to discard targets that are no longer needed.

test_that("load_dependencies in full build", {
  dclean()
  
  # workflow with lots of nested deps
  # This will fail if load_dependencies() doesn't work.
  datasets = plan(x = 1, y = 2, z = 3)
  methods = plan(a = ..dataset.., b = ..dataset.., c = ..dataset..)
  analyses = analyses(methods, datasets)
  heuristics = plan(s = c(..dataset.., ..analysis..), 
    t = ..analysis..)
  summaries = summaries(heuristics, datasets = datasets, 
    analyses = analyses, gather = c("rbind", "rbind"))
  output = plan(final1 = mean(s)+mean(t), 
    final2 = mean(s)-mean(t), 
    waitforme = c(a_x, c_y, s_b_x, t_a_z),
    waitformetoo = c(waitforme, y))
  plan = rbind(datasets, analyses, summaries, output)
  
  # set up a workspace to test load_dependencies()
  config = config(plan, targets = plan$target, 
    envir = new.env(parent = globalenv()), 
    parallelism = "mclapply", jobs = 1, prepend = character(0),
    verbose = TRUE, packages = character(0), 
    prework = character(0), 
    command = "make", args = character(0))
  
  # actually run
  testrun(config)
  expect_true(all(plan$target %in% cached()))
  
  # Check that the right things are loaded and kept
  expect_equal(ls(config$envir), character(0))
  load_dependencies(datasets$target, config)
  expect_equal(ls(config$envir), character(0))
  load_dependencies(analyses$target, config)
  expect_equal(ls(config$envir), c("x", "y", "z"))
  load_dependencies("waitforme", config)
  
  # keep y around for waitformetoo
  expect_equal(ls(config$envir),
    c("a_x", "c_y", "s_b_x", "t_a_z", "y"))
  
  dclean()
})
