# Troubleshooting

Note: `run(..., parallelism = "single-session", jobs = 2)` does not work on Windows because this functionality is based on `parallel::mclapply()`. For `parallelism = "distributed"`, Windows users need [`Rtools`](https://cran.r-project.org/bin/windows/Rtools/) because drake runs `system2("make", ...)`.

If you encounter problems with the package, please read the [`drake` issues](https://github.com/wlandau-lilly/drake/issues) first, taking care to search the closed issues as well. Please submit bug reports, questions, and feature reqests as [`drake` issues](https://github.com/wlandau-lilly/drake/issues).
