# Version 5.4.0.9001

- Add a proper [`clustermq`](https://github.com/mschubert/clustermq)-based parallel backend: `make(parallelism = "clustermq")`.
- Avoid serialization in `digest()` wherever possible. This puts old `drake` projects out of date, but it improves speed.
- Smooth the edges in `vis_drake_graph()` and `render_drake_graph()`.
- Make hover text slightly more readable in in `vis_drake_graph()` and `render_drake_graph()`.
- Improve `dependency_profile()`: show changes to dependencies and major trigger hashes in a nice `tibble`.
- Align hover text properly in `vis_drake_graph()` using the "title" node column.
- Optionally collapse nodes into clusters with `vis_drake_graph(collapse = TRUE)`.
- Choose more appropriate places to check that the `txtq` package is installed.
- Expose the `template` argument of `clustermq` functions (e.g. `Q()` and `workers()`) as an argument of `make()` and `drake_config()`.
- Improve the help files of `loadd()` and `readd()`, giving specific usage guidance in prose.
- Bugfix: `loadd(not_a_target)` no longer loads every target in the cache.
- Bugfix: exclude each target from its own dependency metadata in the "deps" `igraph` vertex attribute (fixes https://github.com/ropensci/drake/issues/503).
- Add a new `code_to_plan()` function to turn R scripts and R Markdown reports into workflow plan data frames.
- Add a new `drake_plan_source()` function, which generates lines of code for a `drake_plan()` call. This `drake_plan()` call produces the plan passed to `drake_plan_source()`. The main purpose is visual inspection (we even have syntax highlighting via `prettycode`) but users may also save the output to a script file for the sake of reproducibility or simple reference.
- Memoize all the steps of `build_drake_graph()` and print to the console the ones that execute.
- Bug fix: avoid `sort(NULL)` because it returns `NA`s on R 3.3.
- Bug fix: work around mysterious `codetools` failures on R 3.3 (add a `tryCatch()` statement in `find_globals()`).
- Skip some tests if `txtq` is not installed.

# Version 5.4.0

- Overhaul the interface for triggers and add new trigger types ("condition" and "change").
- Offload `drake`'s code examples to [this repository](https://github.com/wlandau/drake-examples) and make make `drake_example()` and `drake_examples()` download examples from there.
- Optionally show output files in graph visualizations. See the `show_output_files` argument to `vis_drake_graph()` and friends.
- Repair output file checksum operations for distributed backends like `"clustermq_staged"` and `"future_lapply"`.
- Internally refactor the `igraph` attributes of the dependency graph to allow for smarter dependency/memory management during `make()`.
- Enable `vis_drake_graph()` and `sankey_drake_graph()` to save static image files via `webshot`.
- Deprecate `static_drake_graph()` and `render_static_drake_graph()` in favor of `drake_ggraph()` and `render_drake_ggraph()`.
- Add a `columns` argument to `evaluate_plan()` so users can evaluate wildcards in columns other than the `command` column of `plan`. 
- Name the arguments of `target()` so users do not have to (explicitly).
- Lay the groundwork for a special pretty print method for workflow plan data frames.

# Version 5.3.0

- Allow multiple output files per command.
- Add Sankey diagram visuals: `sankey_drake_graph()` and `render_sankey_drake_graph()`.
- Add `static_drake_graph()` and `render_static_drake_graph()` for `ggplot2`/`ggraph` static graph visualizations.
- Add `group` and `clusters` arguments to `vis_drake_graph()`, `static_drake_graph()`, and `drake_graph_info()` to optionally condense nodes into clusters.
- Implement a `trace` argument to `evaluate_plan()` to optionally add indicator columns to show which targets got expanded/evaluated with which wildcard values.
- Rename the `always_rename` argument to `rename` in `evaluate_plan()`.
- Add a `rename` argument to `expand_plan()`.
- Implement `make(parallelism = "clustermq_staged")`, a `clustermq`-based staged parallelism backend (see https://github.com/ropensci/drake/pull/452).
- Implement `make(parallelism = "future_lapply_staged")`, a `future`-based staged parallelism backend (see https://github.com/ropensci/drake/pull/450).
- Depend on `codetools` rather than `CodeDepends` for finding global variables.
- Detect `loadd()` and `readd()` dependencies in `knitr` reports referenced with `knitr_in()` inside imported functions. Previously, this feature was only available in explicit `knitr_in()` calls in commands.
- Skip more tests on CRAN. White-list tests instead of blacklisting them in order to try to keep check time under the official 10-minute cap.
- Disallow wildcard names to grep-match other wildcard names or any replacement values. This will prevent careless mistakes and confusion when generating `drake_plan()`s.
- Prevent persistent workers from hanging when a target fails.
- Move the example template files to https://github.com/ropensci/drake/tree/master/inst/hpc_template_files.
- Deprecate `drake_batchtools_tmpl_file()` in favor of `drake_hpc_template_file()` and `drake_hpc_template_files()`.
- Add a `garbage_collection` argument to `make()`. If `TRUE`, `gc()` is called after every new build of a target.
- Remove redundant calls to `sanitize_plan()` in `make()`.
- Change `tracked()` to accept only a `drake_config()` object as an argument. Yes, it is technically a breaking change, but it is only a small break, and it is the correct API choice.
- Move visualization and hpc package dependencies to "Suggests:" rather than "Imports:" in the `DESCRIPTION` file.
- Allow processing of codeless `knitr` reports without warnings.

# Version 5.2.1

- Skip several long-running and low-priority tests on CRAN.

# Version 5.2.0

- Sequester staged parallelism in backends "mclapply_staged" and "parLapply_staged". For the other `lapply`-like backends, `drake` uses persistent workers and a master process. In the case of `"future_lapply"` parallelism, the master process is a separate background process called by `Rscript`.
- Remove the appearance of staged parallelism from single-job `make()`'s.
(Previously, there were "check" messages and a call to `staged_parallelism()`.)
- Remove uncontained remnants of staged parallelism internals.
- Allow different parallel backends for imports vs targets. For example, `make(parallelism = c(imports = "mclapply_staged", targets = "mclapply")`.
- Fix a bug in environment pruning. Previously, dependencies of downstream targets were being dropped from memory in `make(jobs = 1)`. Now, they are kept in memory until no downstream target needs them (for `make(jobs = 1)`).
- Improve `predict_runtime()`. It is a more sensible way to go about predicting runtimes with multiple jobs. Likely to be more accurate.
- Calls to `make()` no longer leave targets in the user's environment.
- Attempt to fix a Solaris CRAN check error. The test at https://github.com/ropensci/drake/blob/b4dbddb840d2549621b76bcaa46c344b0fd2eccc/tests/testthat/test-edge-cases.R#L3 was previously failing on CRAN's Solaris machine (R 3.5.0). In the test, one of the threads deliberately quits in error, and the R/Solaris installation did not handle this properly. The test should work now because it no longer uses any parallelism.
- Deprecate the `imports_only` argument to `make()` and `drake_config()` in favor of `skip_targets`.
- Deprecate `migrate_drake_project()`.
- Deprecate `max_useful_jobs()`.
- For non-distributed parallel backends, stop waiting for all the imports to finish before the targets begin.
- Add an `upstream_only` argument to `failed()` so users can list failed targets that do not have any failed dependencies. Naturally accompanies `make(keep_going = TRUE)`.
- Add an RStudio R Markdown template compatible with https://krlmlr.github.io/drake-pitch/.
- Remove `plyr` as a dependency.
- Handle duplicated targets better in `drake_plan()` and `bind_plans()`.
- Add a true function `target()` to help create drake plans with custom columns.
- In `drake_gc()`, clean out disruptive files in `storr`s with mangled keys (re: https://github.com/ropensci/drake/issues/198).
- Move all the vignettes to the up and coming user manual: https://ropenscilabs.github.io/drake-manual/
- Rename the "basic example" to the "mtcars example".
- Deprecate `load_basic_example()` in favor of `load_mtcars_example()`.
- Refocus the `README.md` file on the main example rather than the mtcars example.
- Use a `README.Rmd` file to generate `README.md`.
- Add function `deps_targets()`.
- Deprecate function `deps()` in favor of `deps_code()`
- Add a `pruning_strategy` argument to `make()` and `drake_config()` so the user can decide how `drake` keeps non-import dependencies in memory when it builds a target.
- Add optional custom (experimental) "workers" and "priorities" columns to the `drake` plans to help users customize scheduling.
- Add a `makefile_path` argument to `make()` and `drake_config()` to avoid potential conflicts between user-side custom `Makefile`s and the one written by `make(parallelism = "Makefile")`.
- Document batch mode for long workflows in the HPC guide.
- Add a `console` argument to `make()` and `drake_config()` so users can redirect console output to a file.
- Make it easier for the user to find out where a target in the cache came from: `show_source()`, `readd(show_source = TRUE)`, `loadd(show_source = TRUE)`.

# Version 5.1.2

- In R 3.5.0, the `!!` operator from tidyeval and `rlang` is parsed differently than in R <= 3.4.4. This change broke one of the tests in `tests/testthat/tidy-eval.R` The main purpose of `drake`'s 5.1.2 release is to fix the broken test.
- Fix an elusive `R CMD check` error from building the pdf manual with LaTeX.
- In `drake_plan()`, allow users to customize target-level columns using `target()` inside the commands.
- Add a new `bind_plans()` function to concatenate the rows of drake plans and then sanitize the aggregate plan.
- Add an optional `session` argument to tell `make()` to build targets in a separate, isolated master R session. For example, `make(session = callr::r_vanilla)`.

# Version 5.1.0

- Add a `reduce_plan()` function to do pairwise reductions on collections of targets.
- Forcibly exclude the dot (`.`) from being a dependency of any target or import. This enforces more consistent behavior in the face of the current static code analysis funcionality, which sometimes detects `.` and sometimes does not.
- Use `ignore()` to optionally ignore pieces of workflow plan commands and/or imported functions. Use `ignore(some_code)` to
    1. Force `drake` to not track dependencies in `some_code`, and
    2. Ignore any changes in `some_code` when it comes to deciding which target are out of date.
- Force `drake` to only look for imports in environments inheriting from `envir` in `make()` (plus explicitly namespaced functions).
- Force `loadd()` to ignore foreign imports (imports not explicitly found in `envir` when `make()` last imported them).
- Reduce default verbosity. Only targets are printed out by default. Verbosity levels are integers ranging from 0 through 4.
- Change `loadd()` so that only targets (not imports) are loaded if the `...` and `list` arguments are empty.
- Add check to drake_plan() to check for duplicate targets
- Add a `.gitignore` file containing `"*"` to the default `.drake/` cache folder every time `new_cache()` is called. This means the cache will not be automatically committed to git. Users need to remove `.gitignore` file to allow unforced commits, and then subsequent `make()`s on the same cache will respect the user's wishes and not add another `.gitignore`. this only works for the default cache. Not supported for manual `storr`s.
- Add a new experimental `"future"` backend with a manual scheduler.
- Implement `dplyr`-style `tidyselect` functionality in `loadd()`, `clean()`, and `build_times()`. For `build_times()`, there is an API change: for `tidyselect` to work, we needed to insert a new `...` argument as the first argument of `build_times()`.
- Deprecate the single-quoting API for files. Users should now use formal API functions in their commands:
    - `file_in()` for file inputs to commands or imported functions (for imported functions, the input file needs to be an imported file, not a target).
    - `file_out()` for output file targets (ignored if used in imported functions).
    - `knitr_in()` for `knitr`/`rmarkdown` reports. This tells `drake` to look inside the source file for target dependencies in code chunks (explicitly referenced with `loadd()` and `readd()`). Treated as a `file_in()` if used in imported functions.
- Change `drake_plan()` so that it automatically fills in any target names that the user does not supply. Also, any `file_out()`s become the target names automatically (double-quoted internally).
- Make `read_drake_plan()` (rather than an empty `drake_plan()`) the default `plan` argument in all functions that accept a `plan`.
- Add support for active bindings: `loadd(..., lazy = "bind")`. That way, when you have a target loaded in one R session and hit `make()` in another R session, the target in your first session will automatically update.
- Use tibbles for workflow plan data frames and the output of `dataframes_graph()`.
- Return warnings, errors, and other context of each build, all wrapped up with the usual metadata. `diagnose()` will take on the role of returning this metadata.
- Deprecate the `read_drake_meta()` function in favor of `diagnose()`.
- Add a new `expose_imports()` function to optionally force `drake` detect deeply nested functions inside specific packages.
- Move the "quickstart.Rmd" vignette to "example-basic.Rmd". The so-called "quickstart" didn't end up being very quick, and it was all about the basic example anyway.
- Move `drake_build()` to be an exclusively user-side function.
- Add a `replace` argument to `loadd()` so that objects already in the user's eOne small thing:nvironment need not be replaced.
- When the graph cyclic, print out all the cycles.
- Prune self-referential loops (and duplicate edges) from the workflow graph. That way, recursive functions are allowed.
- Add a `seed` argument to `make()`, `drake_config()`, and `load_basic_example()`. Also hard-code a default seed of `0`. That way, the pseudo-randomness in projects should be reproducible
across R sessions.
- Cache the pseudo-random seed at the time the project is created and use that seed to build targets until the cache is destroyed.
- Add a new `drake_read_seed()` function to read the seed from the cache. Its examples illustrate what `drake` is doing to try to ensure reproducible random numbers.
- Evaluate the quasiquotation operator `!!` for the `...` argument to `drake_plan()`. Suppress this behavior using `tidy_evaluation = FALSE` or by passing in commands passed through the `list` argument.
- Preprocess workflow plan commands with `rlang::expr()` before evaluating them. That means you can use the quasiquotation operator `!!` in your commands, and `make()` will evaluate them according to the tidy evaluation paradigm.
- Restructure `drake_example("basic")`, `drake_example("gsp")`, and `drake_example("packages")` to demonstrate how to set up the files for serious `drake` projects. More guidance was needed in light of [this issue](https://github.com/ropensci/drake/issues/193).
- Improve the examples of `drake_plan()` in the help file (`?drake_plan`).

# Version 5.0.0

- Transfer `drake` to rOpenSci: https://github.com/ropensci/drake
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
- Document triggers and other debugging/testing tools in the new "debug" vignette.
- Restructure the internals of the `storr` cache in a way that is not back-compatible with projects from versions 4.4.0 and earlier. The main change is to make more intelligent use of `storr` namespaces, improving efficiency (both time and storage) and opening up possibilities for new features. If you attempt to run drake >= 5.0.0 on a project from drake <= 4.0.0, drake will stop you before any damage to the cache is done, and you will be instructed how to migrate your project to the new drake.
- Use `formatR::tidy_source()` instead of `parse()` in `tidy_command()` (originally `tidy()` in `R/dependencies.R`). Previously, `drake` was having problems with an edge case: as a command, the literal string `"A"` was interpreted as the symbol `A` after tidying. With `tidy_source()`, literal quoted strings stay literal quoted strings in commands. This may put some targets out of date in old projects, yet another loss of back compatibility in version 5.0.0.
- Speed up clean() by refactoring the cache inventory and using light parallelism.
- Implement `rescue_cache()`, exposed to the user and used in `clean()`. This function removes dangling orphaned files in the cache so that a broken cache can be cleaned and used in the usual ways once more.
- Change the default `cpu` and `elapsed` arguments of `make()` to `NULL`. This solves an elusive bug in how drake imposes timeouts.
- Allow users to set target-level timeouts (overall, cpu, and elapsed) with columns in the workflow plan data frame.
- Document timeouts and retries in the new "debug" vignette.
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
- Add `drake_cache_log()`, `drake_cache_log_file()`, and `make(..., cache_log_file = TRUE)` as options to track changes to targets/imports in the drake cache.
- Detect knitr code chunk dependencies in response to commands with `rmarkdown::render()`, not just `knit()`.
- Add a new general best practices vignette to clear up misconceptions about how to use `drake` properly.

# Version 4.4.0

- Extend `plot_graph()` to display subcomponents. Check out arguments `from`, `mode`, `order`, and `subset`. The graph visualization vignette has demonstrations.
- Add `"future_lapply"` parallelism: parallel backends supported by the [future](https://github.com/HenrikBengtsson/future) and [future.batchtools](https://github.com/HenrikBengtsson/future.batchtools) packages. See `?backend` for examples and the parallelism vignette for an introductory tutorial. More advanced instruction can be found in the `future` and `future.batchtools` packages themselves.
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

# Version 4.3.0: 2017-10-17

Version 4.3.0 has:
- [Reproducible random numbers](https://github.com/ropensci/drake/pull/56)
- [Automatic detection of knitr dependencies](https://github.com/ropensci/drake/issues/9)
- More vignettes
- Bugfixes

# Version 4.2.0: 2017-09-29

Version 4.2.0 will be released today. There are several improvements to code style and performance. In addition, there are new features such as cache/hash externalization and runtime prediction. See the new storage and timing vignettes for details. This release has automated checks for back-compatibility with existing projects, and I also did manual back compatibility checks on serious projects.

# Version 3.0.0: 2017-05-03

Version 3.0.0 is coming out. It manages environments more intelligently so that the behavior of `make()` is more consistent with evaluating your code in an interactive session.

# Version 1.0.1: 2017-02-28

Version 1.0.1 is on CRAN! I'm already working on a massive update, though. 2.0.0 is cleaner and more powerful.

