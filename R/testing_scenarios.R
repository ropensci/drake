local_env_code <-  "new.env(parent = globalenv())"
global_env_code <- "globalenv()"

testing_scenarios <- list(
  local_parL_2 = list(
    envir = local_env_code,
    parallelism = "parLapply",
    jobs = 2,
    cran = TRUE
  ),
  local_parL_1 = list(
    envir = local_env_code,
    parallelism = "parLapply",
    jobs = 1
  ),
  local_mcl_1 = list(
    envir = local_env_code,
    parallelism = "mclapply",
    jobs = 1
  ),
  local_mcl_9 = list(
    envir = local_env_code,
    parallelism = "mclapply",
    jobs = 9,
    skip_os = "windows"
  ),
  local_Make_2 = list(
    envir = local_env_code,
    parallelism = "Makefile",
    jobs = 2
  ),
  local_Make_9 = list(
    envir = local_env_code,
    parallelism = "Makefile",
    jobs = 9
  ),
  global_parL_1 = list(
    envir = global_env_code,
    parallelism = "parLapply",
    jobs = 1
  ),
  global_parL_2 = list(
    envir = global_env_code,
    parallelism = "parLapply",
    jobs = 2
  ),
  global_mcl_1 = list(
    envir = global_env_code,
    parallelism = "mclapply",
    jobs = 1
  ),
  global_mcl_2 = list(
    envir = global_env_code,
    parallelism = "mclapply",
    jobs = 8,
    skip_os = "windows"
  ),
  global_Make_2 = list(
    envir = global_env_code,
    parallelism = "Makefile",
    jobs = 2
  )
)
