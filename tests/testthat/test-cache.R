# library(testthat); library(devtools); load_all()
context("cache")

test_that("cache functions work", {
  dclean()
  expect_equal(character(0), 
    cached(search = FALSE), imported(search = FALSE), 
    built(search = FALSE))
  expect_error(status(search = FALSE))
  expect_error(readd(search = FALSE))
  config = dbug()
  testrun(config)

  # targets
  all = sort(c("'input.rds'", "'intermediatefile.rds'", "a",
        "b", "c", "combined", "f", "final", "g", "h", "i",
        "j", "myinput", "nextone", "readRDS", "saveRDS",
        "yourinput"))
  imports = sort(c("'input.rds'",  "a",
          "b", "c", "f", "g", "h", "i",
          "j", "readRDS", "saveRDS"))
  builds = setdiff(all, imports)
  
  # find stuff in current directory
  # session, status
  expect_true(is.list(session(search = FALSE)))
  expect_true(all(status(search = FALSE) == "finished"))
  expect_equal(sort(names(status(search = FALSE))), all)
  expect_equal(sort(names(status(search = FALSE, 
    imported_files_only = TRUE))), sort(c("'input.rds'", builds)))
  expect_equal(status(bla, f, list = c("h", "final"), search = FALSE), 
    c(bla = "not built or imported", f = "finished", 
      h = "finished", final = "finished"))
  
  # config
  newconfig = read_config(search = FALSE)
  expect_true(is.list(newconfig) & length(newconfig) > 1)
  expect_equal(read_plan(search = FALSE), config$plan)
  expect_equal(class(read_graph(plot = FALSE, 
    search = FALSE)), "igraph")
  pdf(NULL)
  read_graph(plot = TRUE, search = FALSE)
  dev.off()
  unlink("Rplots.pdf")
  
  # imported , built, cached
  expect_equal(imported(files_only = FALSE, search = FALSE), imports)
  expect_equal(imported(files_only = TRUE, search = FALSE), "'input.rds'")
  expect_equal(sort(built(search = FALSE)), sort(config$plan$target))
  twopiece = sort(c(built(search = FALSE), 
    imported(search = FALSE, files_only = FALSE)))
  expect_equal(sort(cached(search = FALSE)), sort(all), twopiece)
  expect_equal(sort(cached(search = FALSE, no_imported_objects = TRUE)), 
    sort(c("'input.rds'", builds)))
  expect_true(all(cached(search = FALSE, list = all)))
  expect_equal(length(cached(search = FALSE, i, 
    list = imported(files_only = FALSE))), 
    length(imported(files_only = FALSE)))
  expect_equal(sort(cached(i, bla, list = c("final", "run"), 
    search = FALSE)),
    sort(c(i = TRUE, bla = FALSE, final = TRUE, run = FALSE)))
  
  # find your project
  expect_equal(find_project(), getwd())
  expect_equal(find_cache(), file.path(getwd(), cachepath))
  expect_true(is.numeric(readd(a, search = FALSE)))
  expect_error(h(1))
  
  # load and read stuff
  expect_true(is.numeric(readd(final, search = FALSE)))
  expect_error(loadd(yourinput, myinput, 
    search = FALSE, imported_only = TRUE))
  loadd(h, i, j, c, search = FALSE)
  expect_true(is.numeric(h(1)))
  rm(h, i, j, c)
  expect_error(h(1))
  
  # test loadd imported_only and loadd() everything
  loadd(imported_only = TRUE)
  expect_true(all(imported(search = FALSE) %in% ls()))
  loadd(search = FALSE)
  expect_true(all(config$cache$list() %in% ls()))
  rm(list = intersect(all, ls()))
  
  # search from a different directory
  if(!file.exists("searchfrom")){
    dir.create("searchfrom")
    dir.create(file.path("searchfrom", "here"))
  }
  setwd("..")
  s = file.path("testthat", "searchfrom", "here")
  
  # status, session 
  expect_true(is.list(session(search = T, path = s)))
  expect_equal(sort(names(status(search = T, path = s))), sort(all))
  expect_equal(sort(names(status(imported_files_only = TRUE, 
    search = T, path = s))), sort(c("'input.rds'", builds)))
  expect_equal(sort(status(search = T, path = s, bla, f, 
    list = c("h", "final"))), 
    sort(c(bla = "not built or imported", f = "finished", 
    h = "finished", final = "finished")))
  
  # imported, built, cached
  expect_equal(sort(imported(files_only = FALSE, search = T, path = s)),
    sort(imports))
  expect_equal(imported(files_only = T, search = T, path = s), 
    "'input.rds'")
  expect_equal(sort(built(search = T, path = s)), 
    sort(config$plan$target))
  twopiece = sort(c(built(path = s, search = T), 
    imported(files_only = FALSE, path = s, search = T)))
  expect_equal(sort(cached(path = s, search = T)), sort(all), twopiece)
  expect_equal(sort(cached(no_imported_objects = TRUE,
    path = s, search = T)), sort(c("'input.rds'", builds)))
  expect_true(all(cached(list = all, path = s, search = T)))
  
  # find your project
  expect_equal(find_project(path = s), "testthat")
  expect_equal(find_cache(path = s), 
    file.path("testthat", cachepath))
  
  # config
  newconfig = read_config(search = TRUE, path = s)
  expect_true(is.list(newconfig) & length(newconfig) > 1)
  expect_equal(read_plan(search = TRUE, path = s), config$plan)
  expect_equal(class(read_graph(plot = FALSE, 
    search = TRUE, path = s)), "igraph")
  pdf(NULL)
  read_graph(plot = TRUE, search = TRUE, path = s)
  dev.off()
  unlink("Rplots.pdf")
  
  # load and read stuff
  expect_true(is.numeric(readd(a, path = s, search = T)))
  expect_error(h(1))
  expect_error(j(1))
  loadd(h, i, j, c, path = s, search = T)
  expect_true(is.numeric(h(1)))
  rm(h, i, j, c)
  expect_error(h(1))
  
  # clean using search = TRUE or FALSE
  expect_true(all(all %in% cached(path = s, search = T)))
  clean(final, path = s, search = TRUE)
  expect_true(all(setdiff(all, "final") %in% 
    cached(path = s, search = T)))
  clean(path = s, search = TRUE)
  expect_equal(cached(path = s, search = T), character(0))
  where = file.path("testthat", cachepath)
  expect_true(file.exists(where))
  clean(path = s, search = FALSE, destroy = TRUE)
  expect_true(file.exists(where))
  clean(path = s, search = TRUE, destroy = TRUE)
  expect_false(file.exists(where))
  
  setwd("testthat")
  unlink("searchfrom", recursive = TRUE)
  dclean()
})
