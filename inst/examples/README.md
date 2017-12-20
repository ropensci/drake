# Drake examples

These are the `drake` examples managed by functions `drake_example()` and `drake_examples()`. To add your own example, simply make a new folder in `inst/examples` and put your files inside. For example, you might create a folder called `inst/examples/new_example`. Then, when the user calls `drake_example("new_example")`, your `new_example` and its contents will be copied to the user's working directory. Also, `"new_example"` will be listed in `drake_examples()`.

Examples so far:
- `basic`: An extended basic exmaple, like the [quickstart vignette](https://github.com/wlandau-lilly/drake/blob/master/vignettes/quickstart.Rmd).
- `Docker-psock`: demonstrates how to deploy targets to a [Docker container](https://www.docker.com/what-container) using a specialized PSOCK cluster.
- `gsp`: A concrete example using real econometrics data. It explores the relationships between gross state product and other quantities, and it shows off `drake`'s ability to generate lots of reproducibly-tracked tasks with ease.
- `Makefile-cluster`: uses [Makefiles](https://www.gnu.org/software/make/) to deploy targets to a generic cluster (configurable).
- `packages`: A concrete example using data on R package downloads. It demonstrates how `drake` can refresh a project based on new incoming data without restarting everything from scratch.
- `sge`: uses `"future_lapply"` parallelism to deploy targets to a [Sun/Univa Grid Engine](https://supcom.hgc.jp/english/utili_info/manual/uge.html) cluster. Other clusters are similar. See the [batchtools/inst/templates](https://github.com/mllg/batchtools/tree/master/inst/templates) and [future.batchtools/inst/templates](https://github.com/HenrikBengtsson/future.batchtools/tree/master/inst/templates) for more example `*.tmpl` template files.
- `slurm`: similar to `sge`, but for [SLURM](https://slurm.schedmd.com).
- `torque`: similar to `sge`, but for [TORQUE](http://www.adaptivecomputing.com/products/open-source/torque/).

Regarding the high-performance computing examples, there is no one-size-fits-all `*.tmpl` configuration file for any job scheduler, so we cannot guarantee that the above examples will work for you out of the box. To learn how to configure the files to suit your needs, you should make sure you understand how to use your job scheduler and [batchtools](https://github.com/mllg/batchtools). 

# Please help

If you have experience with [SLURM](http://slurm.schedmd.com/), [TORQUE](http://www.adaptivecomputing.com/products/open-source/torque/), or [Docker](https://www.docker.com/) in particular, please help us test and fix [the examples](https://github.com/wlandau-lilly/drake/tree/master/inst/examples). You can [share your suggestions on the issue tracker](https://github.com/wlandau-lilly/drake/issues) and [submit bugfixes via pull request](https://help.github.com/articles/about-pull-requests/).
