drake_context("with processx")

if (!identical(getOption("drake_no_processx"), TRUE)) {
  test_with_dir("make(session = callr::r_vanilla)", {
    skip_on_cran() # CRAN gets whitelist tests only (check time limits).
    skip_if_not_installed("callr")
    con <- dbug()
    con$envir <- dbug_envir(globalenv())
    ls1 <- ls(envir = con$envir)
    con$session <- callr::r_vanilla
    con$lock_envir <- FALSE
    make_with_config(con)
    expect_equal(sort(justbuilt(con)), sort(con$plan$target))
    ls2 <- ls(envir = con$envir)
    expect_equal(sort(ls1), sort(ls2))
    rm_these <- setdiff(ls(envir = con$envir), ls1)
    if (length(rm_these)) {
      rm(list = rm_these, envir = con$envir)
    }
  })
}
