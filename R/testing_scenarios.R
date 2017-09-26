testing_scenarios <- list(
  local_parL_2 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "parLapply",
    jobs = 2,
    cran = TRUE
  ),
  local_parL_1 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "parLapply",
    jobs = 1
  ),
  local_mcl_1 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "mclapply",
    jobs = 1
  ),
  local_mcl_8 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "mclapply",
    jobs = 8,
    skip_os = "windows"
  ),
  local_Make_1 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "Makefile",
    jobs = 1
  ),
  local_Make_2 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "Makefile",
    jobs = 2
  ),
  local_Make_16 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "Makefile",
    jobs = 16
  ),
  global_parL_1 = list(
    envir = "globalenv()",
    parallelism = "parLapply",
    jobs = 1
  ),
  global_parL_2 = list(
    envir = "globalenv()",
    parallelism = "parLapply",
    jobs = 2
  ),
  global_mcl_1 = list(
    envir = "globalenv()",
    parallelism = "mclapply",
    jobs = 1
  ),
  global_mcl_2 = list(
    envir = "globalenv()",
    parallelism = "mclapply",
    jobs = 8,
    skip_os = "windows"
  ),
  global_Make_16 = list(
    envir = "globalenv()",
    parallelism = "Makefile",
    jobs = 16
  )
)
