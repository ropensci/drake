# Troubleshooting

The "caution" vignette (`vignette("caution")`, also linked from the [CRAN page](https://CRAN.R-project.org/package=drake) under "vignettes") lists some known issues, limitations, and edge cases. Please look there first if you encounter problems or unexpected behavior. If that fails, please search the [issues page](https://github.com/wlandau-lilly/drake/issues), taking care to browse the closed issues as well. If you are still unsatisfied, please submit your bug report, question, or feature reqest as a [new issue](https://github.com/wlandau-lilly/drake/issues/new).

# Experimental features

## Built-in examples

`Drake` has [built-in examples](https://github.com/wlandau-lilly/drake/tree/master/inst/examples) that you can list with `examples_drake()` and write to your file system with `example_drake()`. Some of these may not work as-is, particularly the ones that require special difficult-to-install software like [SLURM](http://slurm.schedmd.com/) and [TORQUE](http://www.adaptivecomputing.com/products/open-source/torque/). Here is where you can help. If you have experience with these systems, please help us test and fix the examples. You can [share your suggestions on the issue tracker](https://github.com/wlandau-lilly/drake/issues) and [submit bugfixes via pull request](https://help.github.com/articles/about-pull-requests/).
