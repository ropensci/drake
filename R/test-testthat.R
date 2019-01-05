drake_context <- function(x) {
  assert_pkg("testthat")
  ctx <- paste0(get_testing_scenario_name(), ": ", x)
  testthat::context(ctx)
}

testconfig <- function(config) {
  drake_config(
    plan = config$plan,
    targets = config$targets,
    envir = config$envir,
    verbose = config$verbose,
    parallelism = config$parallelism,
    jobs = config$jobs,
    packages = config$packages,
    prework = config$prework,
    prepend = config$prepend,
    command = config$command,
    cache = config$cache,
    lazy_load = config$lazy_load,
    session_info = config$session_info,
    fetch_cache = config$fetch_cache,
    caching = config$caching,
    lock_envir = !any(grepl("staged", config$parallelism))
  )
}

testrun <- function(config) {
  set_test_backend()
  invisible(
    make(
      plan = config$plan,
      targets = config$targets,
      envir = config$envir,
      verbose = config$verbose,
      parallelism = config$parallelism,
      jobs = config$jobs,
      packages = config$packages,
      prework = config$prework,
      prepend = config$prepend,
      command = config$command,
      cache = config$cache,
      lazy_load = config$lazy_load,
      session_info = config$session_info,
      fetch_cache = config$fetch_cache,
      caching = config$caching,
      lock_envir = !any(grepl("staged", config$parallelism))
    )
  )
}

justbuilt <- function(config) {
  recorded <- config$cache$list(namespace = "progress")
  all <- lightly_parallelize(
    X = recorded,
    FUN = function(target) {
      config$cache$get(
        key = target, namespace = "progress", use_cache = FALSE)
    },
    jobs = config$jobs
  )
  names(all) <- recorded
  all <- unlist(all)
  out <- Filter(
    all,
    f = function(x) {
      x == "finished"
    }
  )
  sort(intersect(names(out), y = config$plan$target))
}

nobuild <- function(config) {
  assert_pkg("testthat")
  testthat::expect_true(length(justbuilt(config)) < 1)
}

#' @title Run a unit test in a way that quarantines
#'   the side effects from your workspace and file system.
#' @description Typical users of drake should not need this function.
#' It is exported so it can be used to quarantine the side effects
#' of the examples in the help files.
#' @export
#' @keywords internal
#' @return Nothing.
#' @param desc character, description of the test
#' @param ... code to test
#' @examples
#' \dontrun{
#' test_with_dir(
#'   "Write a file to a temporary folder",
#'   writeLines("hello", "world.txt")
#' )
#' file.exists("world.txt") # FALSE
#' }
test_with_dir <- function(desc, ...) {
  assert_pkg("testthat")
  old <- Sys.getenv("drake_warn_subdir")
  Sys.setenv(drake_warn_subdir = "false")
  on.exit(Sys.setenv(drake_warn_subdir = old))
  while (file.exists(new <- tempfile())) {
    # Should not reach this part of the loop.
    Sys.sleep(0.01) # nocov
  }
  dir.create(new)
  with_dir(new = new, {
    with_options(new = list(clustermq.scheduler = "multicore"), {
      set_test_backend()
      testthat::test_that(desc = desc, ...)
    })
  })
  invisible()
}

restore_options <- function(old) {
  current <- options()
  remove_these <- setdiff(names(current), names(old))
  removal_list <- as.list(old[remove_these])
  names(removal_list) <- remove_these
  do.call(options, removal_list)
  options(old)
}

set_test_backend <- function() {
  eval(parse(text = get_testing_scenario()$backend))
}

unit_test_files <- function(path = getwd(), max_depth = 100) {
  # find the package root
  p <- normalizePath(dirname(path))
  criterion <- "DESCRIPTION"
  for (i in seq_len(max_depth)) {
    if (length(list.files(p, pattern = criterion))) {
      # found criterion file; make sure it's ours
      if (any(grepl("^Package: drake$", readLines(file.path(p, criterion))))) {
        return(file.path(p, "tests", "testthat"))
      }
    }
    p <- dirname(p)
  }
  stop("Maximum search of ", max_depth, " exceeded for ", path)
}

with_all_options <- function(code) {
  old <- options()
  on.exit(restore_options(old))
  force(code)
}

write_v6.2.1_project <- function() { # nolint
  zip <- system.file(
    file.path("testing", "built_mtcars_example_v6.2.1.zip"),
    package = "drake",
    mustWork = TRUE
  )
  unzip(zip, exdir = ".", setTimes = TRUE)
}

# Some installations of R require the && and || operators
# to return a result of length 1.
# For example, `nzchar(letters) && length(letters)` fails on
# some platforms but not others. Below, we mock the operators
# to preempt these elusive failures.
# Below, toggle the if() condition on in test mode
# and off in production mode.
# nocov start
if (FALSE) {
  `&&` <- function(x, y) {
    if (length(x) != 1) {
      stop("length x not 1")
    } else if (!x) {
      return(x)
    }
    if (length(y) != 1) {
      stop("length y not 1")
    }
    y
  }

  `||` <- function(x, y) {
    if (length(x) != 1) {
      stop("length x not 1")
    } else if (x) {
      return(x)
    }
    if (length(y) != 1) {
      stop("length y not 1")
    }
    y
  }
}
# nocov end
