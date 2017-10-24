These are the `drake` examples managed by functions `example_drake()` and `examples_drake()`. To add your own example, simply make a new folder in `inst/examples` and put your files inside. For example, you might create a folder called `inst/examples/new_example`. Then, when the user calls `example_drake("new_example")`, your `new_example` and its contents will be copied to the user's working directory. Also, `"new_example"` will be listed in `examples_drake()`.

Examples so far:
- `basic`: An extended basic exmaple, like the [quickstart vignette](https://github.com/wlandau-lilly/drake/blob/master/vignettes/quickstart.Rmd).
- `Makefile-cluster`: uses [Makefiles](https://www.gnu.org/software/make/) to deploy targets to a generic cluster (configurable).
- `sge`: uses `"future_lapply"` parallelism to deploy targets to a [Sun/Univa Grid Engine](https://supcom.hgc.jp/english/utili_info/manual/uge.html) cluster. Other clusters are similar. See the [batchtools GitHub repository](https://github.com/mllg/batchtools/tree/master/inst/templates) for `*.tmpl` template files for other clusters.
- `slurm`: similar to `sge`, but for [SLURM](https://slurm.schedmd.com).
