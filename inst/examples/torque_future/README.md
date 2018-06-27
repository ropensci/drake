This example demonstrates how to deploy `drake` targets to
separate jobs on a [TORQUE cluster](http://www.adaptivecomputing.com/products/open-source/torque/).
For more examples of `future.batchtools`, visit [the GitHub page](https://github.com/HenrikBengtsson/future.batchtools).
 See the [batchtools/inst/templates](https://github.com/mllg/batchtools/tree/master/inst/templates) and [future.batchtools/inst/templates](https://github.com/HenrikBengtsson/future.batchtools/tree/master/inst/templates) on [GitHub](https://github.com/) for more example `*.tmpl` template files.

When you are ready, use the following Linux terminal command to run the example.
The jobs will be managed in a persistent background process.

```
nohup nice -4 R CMD BATCH run.R &
```
