# Troubleshooting

## Help

If you encounter problems with the package, please read the [`drake` issues](https://github.com/wlandau-lilly/drake/issues) first, taking care to search the closed issues as well. Please submit bug reports, questions, and feature reqests as [`drake` issues](https://github.com/wlandau-lilly/drake/issues).


## Some known issues

Drake tries to reproducibly track everything, but there are limitations. It is possible in some cases to trick drake into ignoring dependencies. Please read the "caution" vignette (`vignette("caution")`) to use drake safely.

`run(..., parallelism = "mclapply", jobs = 2)` does not work on Windows because this functionality is based on `parallel::mclapply()`. For `parallelism = "Makefile"`, Windows users need [`Rtools`](https://cran.r-project.org/bin/windows/Rtools/) because drake runs `system2("make", ...)`.
