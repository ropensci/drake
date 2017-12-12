# Release 5.0.0

- Several functions now require an explicit `config` argument, which you can get from
`drake_config()` or `make()`. Examples:
    - outdated()
    - missed()
    - rate_limiting_times()
    - predict_runtime()
    - vis_drake_graph()
    - dataframes_graph()
- Always process all the imports before building any targets. This is part of the solution to #168: if imports and targets are processed together, the full power of parallelism is taken away from the targets. Also, the way parallelism happens is now consistent for all parallel backends.
- Major speed improvement: dispense with internal inventories and rely on `cache$exists()` instead.
- Let the user define a trigger for each target to customize when `make()` decides to build targets.
- Document triggers and other debugging/testing tools in the new [debug vignette](https://github.com/wlandau-lilly/drake/blob/master/vignettes/debug.Rmd).
- Restructure the internals of the `storr` cache in a way that is not back-compatible with projects from versions 4.4.0 and earlier. The main change is to make more intelligent use of `storr` namespaces, improving efficiency (both time and storage) and opening up possibilities for new features. If you attempt to run drake >= 5.0.0 on a project from drake <= 4.0.0, drake will stop you before any damage to the cache is done, and you will be instructed how to migrate your project to the new drake.
- Use `formatR::tidy_source()` instead of `parse()` in `tidy_command()` (originally `tidy()` in `R/dependencies.R`). Previously, `drake` was having problems with an edge case: as a command, the literal string `"A"` was interpreted as the symbol `A` after tidying. With `tidy_source()`, literal quoted strings stay literal quoted strings in commands. This may put some targets out of date in old projects, yet another loss of back compatibility in version 5.0.0.
- Speed up clean() by refactoring the cache inventory and using light parallelism.
- Implement `rescue_cache()`, exposed to the user and used in `clean()`. This function removes dangling orphaned files in the cache so that a broken cache can be cleaned and used in the usual ways once more.
- Change the default `cpu` and `elapsed` arguments of `make()` to `NULL`. This solves an elusive bug in how drake imposes timeouts.
- Allow users to set target-level timeouts (overall, cpu, and elapsed) with columns in the workflow plan data frame.
- Document timeouts and retries in the new [debug vignette](https://github.com/wlandau-lilly/drake/blob/master/vignettes/debug.Rmd).
- Add a new `graph` argument to functions `make()`, `outdated()`, and `missed()`.
- Export a new `prune_graph()` function for igraph objects.
- Delete long-deprecated functions `prune()` and `status()`.
- Deprecate and rename functions:
    - `analyses()` => `plan_analyses()`
    - `as_file()` => `as_drake_filename()`
    - `backend()` => `future::plan()`
    - `build_graph()` => `build_drake_graph()`
    - `check()` => `check_plan()`
    - `config()` => `drake_config()`
    - `evaluate()` => `evaluate_plan()`
    - `example_drake()` => `drake_example()`
    - `examples_drake()` => `drake_examples()`
    - `expand()` => `expand_plan()`
    - `gather()` => `gather_plan()`
    - `plan()`, `workflow()`, `workplan()` => `drake_plan()`
    - `plot_graph()` => `vis_drake_graph()`
    - `read_config()` => `read_drake_config()`
    - `read_graph()` => `read_drake_graph()`
    - `read_plan()` => `read_drake_plan()`
    - `render_graph()` => `render_drake_graph()`
    - `session()` => `drake_session()`
    - `summaries()` => `plan_summaries()`
- Disallow `output` and `code` as names in the workflow plan data frame. Use `target` and `command` instead. This naming switch has been formally deprecated for several months prior.
- Deprecate the ..analysis.. and ..dataset.. wildcards in favor of analysis__ and dataset__, respectively. The new wildcards are stylistically better an pass linting checks.
- Add new functions `drake_quotes()`, `drake_unquote()`, and `drake_strings()` to remove the silly dependence on the `eply` package.
- Add a `skip_safety_checks` flag to `make()` and `drake_config()`. Increases speed.
- In `sanitize_plan()`, remove rows with blank targets "".
- Add a `purge` argument to `clean()` to optionally remove all target-level information.
- Add a `namespace` argument to `cached()` so users can inspect individual `storr` namespaces.
- Change `verbose` to numeric: 0 = print nothing, 1 = print progress on imports only, 2 = print everything.
- Add a new `next_stage()` function to report the targets to be made in the next parallelizable stage.
- Add a new `session_info` argument to `make()`. Apparently, `sessionInfo()` is a bottleneck for small `make()`s, so there is now an option to suppress it. This is mostly for the sake of speeding up unit tests.
- Add a new `log_progress` argument to `make()` to suppress progress logging. This increases storage efficiency and speeds some projects up a tiny bit.
- Add an optional `namespace` argument to `loadd()` and `readd()`. You can now load and read from non-default `storr` namespaces.

# Changes in release 4.4.0

- Extend `plot_graph()` to display subcomponents. Check out arguments `from`, `mode`, `order`, and `subset`. The [graphing vignette](https://github.com/wlandau-lilly/drake/blob/master/vignettes/graph.Rmd) has demonstrations.
- Add `"future_lapply"` parallelism: parallel backends supported by the [future](https://github.com/HenrikBengtsson/future) and [future.batchtools](https://github.com/HenrikBengtsson/future.batchtools) packages. See `?backend` for examples and the [parallelism vignette](https://github.com/wlandau-lilly/drake/blob/master/vignettes/parallelism.Rmd) for an introductory tutorial. More advanced instruction can be found in the `future` and `future.batchtools` packages themselves.
- Cache diagnostic information of targets that fail and retrieve diagnostic info with `diagnose()`.
- Add an optional `hook` argument to `make()` to wrap around `build()`. That way, users can more easily control the side effects of distributed jobs. For example, to redirect error messages to a file in `make(..., parallelism = "Makefile", jobs = 2, hook = my_hook)`, `my_hook` should be something like `function(code){withr::with_message_sink("messages.txt", code)}`.
- Remove console logging for "parLapply" parallelism. `Drake` was previously using the `outfile` argument for PSOCK clusters to generate output that could not be caught by `capture.output()`. It was a hack that should have been removed before.
- Remove console logging for "parLapply" parallelism. `Drake` was previously using the `outfile` argument for PSOCK clusters to generate output that could not be caught by `capture.output()`. It was a hack that should have been removed before.
- If 'verbose' is 'TRUE' and all targets are already up to date (nothing to build), then `make()` and `outdated()` print "All targets are already up to date" to the console.
- Add new examples in 'inst/examples', most of them demonstrating how to use the `"future_lapply"` backends.
- New support for timeouts and retries when it comes to building targets.
- Failed targets are now recorded during the build process. You can see them in `plot_graph()` and `progress()`. Also see the new `failed()` function, which is similar to `in_progress()`.
- Speed up the overhead of `parLapply` parallelism. The downside to this fix is that `drake` has to be properly installed. It should not be loaded with `devtools::load_all()`. The speedup comes from lightening the first `clusterExport()` call in `run_parLapply()`. Previously, we exported every single individual `drake` function to all the workers, which created a bottleneck. Now, we just load `drake` itself in each of the workers, which works because `build()` and `do_prework()` are exported. 
- Change default value of `overwrite` to `FALSE` in `load_basic_example()`.
- Warn when overwriting an existing `report.Rmd` in `load_basic_example()`.
- Tell the user the location of the cache using a console message. Happens on every call to `get_cache(..., verbose = TRUE)`.
- Increase efficiency of internal preprocessing via `lightly_parallelize()` and `lightly_parallelize_atomic()`. Now, processing happens faster, and only over the unique values of a vector.
- Add a new `make_with_config()` function to do the work of `make()` on an existing internal configuration list from `drake_config()`.
- Add a new function `drake_batchtools_tmpl_file()` to write a `batchtools` template file from one of the examples (`drake_example()`), if one exists.

# 2017-10-17

Version 4.3.0 has:
- [Reproducible random numbers](https://github.com/wlandau-lilly/drake/pull/56)
- [Automatic detection of knitr dependencies](https://github.com/wlandau-lilly/drake/issues/9)
- More vignettes
- Bugfixes

# 2017-09-29

Version 4.2.0 will be released today. There are several improvements to code style and performance. In addition, there are new features such as cache/hash externalization and runtime prediction. See the new storage and timing vignettes for details. This release has automated checks for back-compatibility with existing projects, and I also did manual back compatibility checks on serious projects.

# 2017-05-03

Version 3.0.0 is coming out. It manages environments more intelligently so that the behavior of `make()` is more consistent with evaluating your code in an interactive session.

# 2017-02-28

Version 1.0.1 is on CRAN! I'm already working on a massive update, though. 2.0.0 is cleaner and more powerful.


# 2017-02-20

Version 1.0.1 will be the first submission to CRAN.
