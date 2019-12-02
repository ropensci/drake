# Version 7.8.0.9000



# Version 7.8.0

## Bug fixes

- Prevent `drake::drake_plan(x = target(...))` from throwing an error if `drake` is not loaded (#1039, @mstr3336).
- Move the `transformations` lifecycle badge to the proper location in the docstring (#1040, @jeroen).
- Prevent `readd()` / `loadd()` from turning an imported function into a target (#1067).
- Align in-memory `disk.frame` targets with their stored values (#1077, @brendanf).

## New features

- Implement dynamic branching (#685).
- Add a new `subtargets()` function to get the cached names of the sub-targets of a dynamic target.
- Add new `subtargets` arguments to `loadd()` and `readd()` to retrieve specific sub-targets from a parent dynamic target.
- Add new `get_trace()` and `read_trace()` functions to help track which values of grouping variables go into the making of dynamic sub-targets.
- Add a new `id_chr()` function to get the name of the target while `make()` is running.
- Implement `plot(plan)` (#1036).
- `vis_drake_graph()`, `drake_graph_info()`, and `render_drake_graph()` now 
  take arguments that allow behavior to be defined upon selection of nodes. (#1031, @mstr3336).
- Add a new `max_expand` argument to `make()` and `drake_config()` to scale down dynamic branching (#1050, @hansvancalster).

## Enhancements

- Document transformation functions in a way that avoids having to create true functions (#979).
- Avoid always invalidating the memoized layout when we set the knitr hash.
- Change the names of environments in `drake_config()` objects.
- Assert that `prework` is a language object, list of language objects, or character vector (#1 at pat-s/multicore-debugging on GitHub, @pat-s).
- Use an environment instead of a list for `config$layout`. Supports internal modifications by reference. Required for #685.
- Clean up the code of the parallel backends.
- Make `dynamic` a formal argument of `target()`.
- Always lock/unlock the environment target by target, allowing informative error messages to appear more readily (#1062, @PedramNavid)
- Automatically ignore `storr`s and decorated `storr`s (#1071).
- Speed up memory management by avoiding a call to `setdiff()` and avoiding `names(config$envir_targets)`.


# Version 7.7.0

## Bug fixes

- Take the sum instead of the max in `dir_size()`. Incurs rehashing for some workflows, but should not invalidate any targets.

## New features

- Add a new `which_clean()` function to preview which targets will be invalidated by `clean()` (#1014, @pat-s).
- Add serious import and export methods for the decorated `storr` (#1015, @billdenney, @noamross).
- Add a new `"diskframe"` format for larger-than-memory data (#1004, @xiaodaigh).
- Add a new `drake_tempfile()` function to help with `"diskframe"` format. It makes sure we are not copying large datasets across different physical storage media (#1004, @xiaodaigh).
- Add new function `code_to_function()` to allow for parsing script based workflows into functions so `drake_plan()` can begin to manage the workflow and track dependencies. (#994, @thebioengineer)

## Enhancements

- Coerce seeds to integers in `seed_trigger()` (#1013, @CreRecombinase).
- Hard wrap long labels in graph visuals (#1017).
- Nest the history `txtq` API inside decorated `storr` API (#1020).
- Reduce cyclomatic complexity of internal functions.
- Reduce retrievals of old target metadata to try to improve performance (#1027).


# Version 7.6.2

## Bug fixes

- Remove README.md from CRAN altogether. Also remove all links from the news and vignette. The links trigger too many CRAN notes, which made the automated checks too brittle.
- Serialize formats that need serialization (like "keras") before sending the data from HPC workers to the master process (#989).
- Check for custom-formatted files when checking checksums.
- Force fst-formatted targets to plain data frames. Same goes for the new "fst_dt" format.
- Change the meaning and behavior of `max_expand` in `drake_plan()`. `max_expand` is now the maximum number of targets produced by `map()`, `split()`, and `cross()`. For `cross()`, this reduces the number of targets (less cumbersome) and makes the subsample of targets more representative of the complete grid. It also. ensures consistent target naming when `.id` is `FALSE` (#1002). Note: `max_expand` is not for production workflows anyway, so this change does not break anything important. Unfortunately, we do lose the speed boost in `drake_plan()` originally due to `max_expand`, but `drake_plan()` is still fast, so that is not so bad.
- Drop specialized formats of `NULL` targets (#998).
- Prevent false grouping variables from partially tagging along in `cross()` (#1009). The same fix should apply to `map()` and `split()` too.
- Respect graph topology when recovering old grouping variables for `map()` (#1010).

## New features

- Add a new "fst_dt" format for `fst`-powered saving of `data.table` objects.
- Support a custom "caching" column of the plan to select master vs worker caching for each target individually (#988).
- Make `transform` a formal argument of `target()` so that users do not have to type "transform =" all the time in `drake_plan()` (#993).
- Migrate the documentation website from `ropensci.github.io/drake` to `docs.ropensci.org/drake`.

## Enhancements

- Document the HPC limitations of `target(format = "keras")` (#989).
- Remove the now-superfluous vignette.
- Wrap up console and text file logging functionality into a reference class (#964).
- Deprecate the `verbose` argument in various caching functions. The location of the cache is now only printed in `make()`. This made the previous feature easier to implement.
- Carry forward nested grouping variables in `combine()` (#1008).
- Improve the encapsulation of hash tables in the decorated `storr` (#968).


# Version 7.6.1

## Bug fixes

- CRAN hotfix: remove a broken link in the README.


# Version 7.6.0

## Bug fixes

- Make `drake_plan(transform = slice())` understand `.id` and grouping variables (#963).
- Repair `clean(garbage_collection = TRUE, destroy = TRUE)`. Previously it destroyed the cache before trying to collect garbage.
- Ensure that `r_make()` passes informative error messages back to the calling process (#969).
- Avoid downloading full contents of URLs when rehashing (#982)
- Retain upstream grouping variables of `map()` and `cross()` on topologically side-by-side targets (#983).
- Manually enforce the correct ordering in `dsl_left_outer_join()` so `cross()` selects the right combinations of existing targets (#986). This bug was probably introduced in the solution to #983.
- Make the output of `progress()` more consistent, less dependent on whether `tidyselect` is installed.

## New features

- Support specialized data storage via a decorated cache and `format` argument of `target()` (#971). This allows users to leverage faster ways to save and load targets, such as `write_fst()` for data frames and `save_model_hdf5()` for Keras models. It also improves memory because it prevents `storr` from making a serialized in-memory copy of large data objects.
- Add `tidyselect` functionality for `...` in `progress()`, analogous to `loadd()`, `build_times()`, and `clean()`.
- Support S3 for user-defined generics (#959). If the generic `do_stuff()` and the method `stuff.your_class()` are defined in `envir`, and if `do_stuff()` has a call to `UseMethod("stuff")`, then `drake`'s code analysis will detect `stuff.your_class()` as a dependency of `do_stuff()`.
- Add authentication support for `file_in()` URLs. Requires the new `curl_handles` argument of `make()` and `drake_config()` (#981).

## Enhancements

- Document DSL keywords as if they were true functions: `target()`, `map()`, `split()`, `cross()`, and `combine()` (#979).
- Do garbage collection between the unloading and loading phases of memory management.
- Keep `file_out()` files in `clean()` unless `garbage_collection` is `TRUE`. That way, `make(recover = TRUE)` is a true "undo button" for `clean()`. `clean(garbage_collection = TRUE)` still removes data in the cache, as well as any `file_out()` files from targets currently being cleaned.
- The menu in `clean()` only appears if `garbage_collection` is `TRUE`. Also, this menu is added to `rescue_cache(garbage_collection = TRUE)`.
- Reorganize the internal code files and functions to make development easier.
- Move the history inside the cache folder `.drake/`. The old `.drake_history/` folder was awkward. Old histories are migrated during `drake_config()`, and `drake_history()`.
- Add lifecycle badges to exported functions.

# Version 7.5.2

## Bug fixes

- Eliminate accidental creations of `.drake_history` in `plan_to_code()`, `plan_to_notebook()`, and the examples in the help files.


# Version 7.5.1

## Bug fixes

- Change \.drake_history$ to ^\.drake_history$ in .Rbuildignore appease CRAN checks. 
- Repair help file examples.


# Version 7.5.0

## New features

- Add automated data recovery (#945). This is still experimental and disabled by default. Requires `make(recover = TRUE)`.
- Add new functions `recoverable()` and `r_recoverable()` to show targets that are outdated but recoverable via `make(recover = TRUE)`.
- Track the history and provenance of targets, viewable with `drake_history()`. Powered by `txtq` (#918, #920).
- Add a new `no_deps()` function, similar to `ignore()`. `no_deps()` suppresses dependency detection but still tracks changes to the literal code (#910).
- Add a new "autoclean" memory strategy (#917).
- Export `transform_plan()`.
- Allow a custom `seed` column of `drake` plans to set custom seeds (#947).
- Add a new `seed` trigger to optionally ignore changes to the target seed (#947).

## Enhancements

- In `drake_plan()`, interpret custom columns as non-language objects (#942).
- Suggest and assert `clustermq` >= 0.8.8.
- Log the target name in a special column in the console log file (#909).
- Rename the "memory" memory strategy to "preclean" (with deprecation; #917).
- Deprecate `ensure_workers` in `drake_config()` and `make()`.
- Warn when the user supplies additional arguments to `make()` after `config` is already supplied.
- Prevent users from running `make()` from inside the cache (#927).
- Add `CITATION` file with JOSS paper.
- In `deps_profile()`, include the seed and change the names.
- Allow the user to set a different seed in `make()`. All this does is invalidate old targets.
- Use `set_hash()` and `get_hash()` in `storr` to double the speed of progress tracking.

## Bug fixes

- In the static code analysis for dependency detection, ignore list elements referenced with `$` (#938).
- Minor: handle strings embedded in language objects (#934).
- Minor: supply `xxhash64` as the default hash algorithm for non-`storr` hashing if the driver does not have a hash algorithm.


# Version 7.4.0

## Mildly breaking changes

These changes are technically breaking changes, but they should only affect advanced users.

- `rescue_cache()` no longer returns a value.

## Bug fixes

- Restore compatibility with `clustermq` (#898). Suggest version >= 0.8.8 but allow 0.8.7 as well.
- Ensure `drake` recomputes `config$layout` when `knitr` reports change (#887).
- Do not rehash large imported files every `make()` (#878).
- Repair parsing of long tidy eval inputs in the DSL (#878).
- Clear up cache confusion when a custom cache exists adjacent to the default cache (#883).
- Accept targets as symbols in `r_drake_build()`.
- Log progress during `r_make()` (#889).
- Repair `expose_imports()`: do not do the `environment<-` trick unless the object is a non-primitive function.
- Use different static analyses of `assign()` vs `delayedAssign()`.
- Fix a superfluous code analysis warning incurred by multiple `file_in()` files and other strings (#896).
- Make `ignore()` work inside `loadd()`, `readd()`, `file_in()`, `file_out()`, and `knitr_in()`.

## New features

- Add experimental support for URLs in `file_in()` and `file_out()`. `drake` now treats `file_in()`/`file_out()` files as URLS if they begin with "http://", "https://", or "ftp://". The fingerprint is a concatenation of the ETag and last-modified timestamp. If neither can be found or if there is no internet connection, `drake` throws an error.
- Implement new memory management strategies `"unload"` and `"none"`, which do not attempt to load a target's dependencies from memory (#897).
- Allow users to give each target its own memory strategy (#897).
- Add `drake_slice()` to help split data across multiple targets. Related: #77, #685, #833.
- Introduce a new `drake_cache()` function, which is now recommended instead of `get_cache()` (#883).
- Introduce a new `r_deps_target()` function.
- Add RStudio addins for `r_make()`, `r_vis_drake_graph()`, and `r_outdated()` (#892).

## Enhancements

- Deprecate `get_cache()` in favor of `drake_cache()`.
- Show the path to the cache in the `clean()` menu prompt.
- Stop removing the console log file on each call to `drake_config()`.
- Log the node name (short host name) and process ID in the console log file.
- Log the name of the calling function in the console log file, e.g. "begin make()" and "end make()". Applies to all functions that accept a `config` argument.
- Memory management: set `use_cache` to `FALSE` in `storr` function calls for saving and loading targets. Also, at the end of `make()`, call `flush_cache()` (and then `gc()` if garbage collection is enabled).
- Mention `callr::r()` within commands as a safe alternative to `lock_envir = FALSE` in the self-invalidation section of the `make()` help file.
- Use file size to help decide when to rehash `file_in()`/`file_out()`/`knitr_in()` files. We now rehash files if the file is less than 100 KB or the time stamp changed or the **file size** changed.

# Version 7.3.0

## Bug fixes

- Accommodate `rlang`'s new interpolation operator `{{`, which was causing `make()` to fail when `drake_plan()` commands are enclosed in curly braces (#864).
- Move "`config$lock_envir <- FALSE`" from `loop_build()` to  `backend_loop()`. This makes sure `config$envir` is correctly locked in `make(parallelism = "clustermq")`.
- Convert factors to characters in the optional `.data` argument of `map()` and `cross()` in the DSL.
- In the DSL of `drake_plan()`, repair `cross(.data = !!args)`, where `args` is an optional data frame of grouping variables.
- Handle trailing slashes in `file_in()`/`file_out()` directories for Windows (#855).
- Make `.id_chr` work with `combine()` in the DSL (#867).
- Do not try `make_spinner()` unless the version of `cli` is at least 1.1.0.

## New features

- Add functions `text_drake_graph()` (and `r_text_drake_graph()` and `render_text_drake_graph()`). Uses text art to print a dependency graph to the terminal window. Handy for when users SSH into remote machines without X Window support.
- Add a new `max_expand` argument to `drake_plan()`, an optional upper bound on the lengths of grouping variables for `map()` and `cross()` in the DSL. Comes in handy when you have a massive number of targets and you want to test on a miniature version of your workflow before you scale up to production.

## Enhancements

- Delay the initialization of `clustermq` workers for as long as possible. Before launching them, build/check targets locally until we reach an outdated target with `hpc` equal to `FALSE`. In other words, if no targets actually require `clustermq` workers, no workers get created.
- In `make(parallelism = "future")`, reset the `config$sleep()` backoff interval whenever a new target gets checked.
- Add a "done" message to the console log file when the workflow has completed.
- Replace `CodeDepends` with a base R solution in `code_to_plan()`. Fixes a CRAN note.
- The DSL (transformations in `drake_plan()`) is no longer experimental.
- The `callr` API (`r_make()` and friends) is no longer experimental.
- Deprecate the wildcard/text-based functions for creating plans: `evaluate_plan()`, `expand_plan()`, `map_plan()`, `gather_plan()`, `gather_by()`, `reduce_plan()`, `reduce_by()`. 
- Change some deprecated functions to defunct: `deps()`, `max_useful_jobs()`, and `migrate_drake_project()`.

# Version 7.2.0

## Mildly breaking changes

- In the DSL (e.g. `drake_plan(x = target(..., transform = map(...)))` avoid inserting extra dots in target names when the grouping variables are character vectors (#847). Target names come out much nicer this way, but those name changes will invalidate some targets (i.e. they need to be rebuilt with `make()`).

## Bug fixes

- Use `config$jobs_preprocess` (local jobs) in several places where `drake` was incorrectly using `config$jobs` (meant for targets).
- Allow `loadd(x, deps = TRUE, config = your_config)` to work even if `x` is not cached (#830). Required disabling `tidyselect` functionality when `deps` `TRUE`. There is a new note in the help file about this, and an informative console message prints out on `loadd(deps = TRUE, tidyselect = TRUE)`. The default value of `tidyselect` is now `!deps`.
- Minor: avoid printing messages and warnings twice to the console (#829).
- Ensure compatibility with `testthat` >= 2.0.1.9000.

## New features

- In `drake_plan()` transformations, allow the user to refer to a target's own name using a special `.id_chr` symbol, which is treated like a character string.
- Add a `transparency` argument to `drake_ggraph()` and `render_drake_ggraph()` to disable transparency in the rendered graph. Useful for R installations without transparency support.

## Enhancements

- Use a custom layout to improve node positions and aspect ratios of `vis_drake_graph()` and `drake_ggraph()` displays. Only activated in `vis_drake_graph()` when there are at least 10 nodes distributed in both the vertical and horizontal directions.
- Allow nodes to be dragged both vertically and horizontally in `vis_drake_graph()` and `render_drake_graph()`.
- Prevent dots from showing up in target names when you supply grouping variables to transforms in `drake_plan()` (#847).
- Do not keep `drake` plans (`drake_plan()`) inside `drake_config()` objects. When other bottlenecks are removed, this will reduce the burden on memory (re #800).
- Do not retain the `targets` argument inside `drake_config()` objects. This is to reduce memory consumption.
- Deprecate the `layout` and `direction` arguments of `vis_drake_graph()` and `render_drake_graph()`. Direction is now always left to right and the layout is always Sugiyama.
- Write the cache log file in CSV format (now `drake_cache.csv` by default) to avoid issues with spaces (e.g. entry names with spaces in them, such as "file report.Rmd")`.


# Version 7.1.0

## Bug fixes

- In `drake` 7.0.0, if you run `make()` in interactive mode and respond to the menu prompt with an option other than `1` or `2`, targets will still build. 
- Make sure file outputs show up in `drake_graph()`. The bug came from `append_output_file_nodes()`, a utility function of `drake_graph_info()`.
- Repair `r_make(r_fn = callr::r_bg())` re #799.
- Allow `drake_ggraph()` and `sankey_drake_graph()` to work when the graph has no edges.

## New features

- Add a new `use_drake()` function to write the `make.R` and `_drake.R` files from the "main example". Does not write other supporting scripts.
- With an optional logical `hpc` column in your `drake_plan()`, you can now select which targets to deploy to HPC and which to run locally.
- Add a `list` argument to `build_times()`, just like `loadd()`.
- Add a new RStudio addin: 'loadd target at cursor' which can be bound a keyboard shortcut to load the target identified by the symbol at the cursor position to the global environment.

## Enhancements

- `file_in()` and `file_out()` can now handle entire directories, e.g. `file_in("your_folder_of_input_data_files")` and `file_out("directory_with_a_bunch_of_output_files")`.
- Send less data from `config` to HPC workers.
- Improve `drake_ggraph()`
  - Hide node labels by default and render the arrows behind the nodes.
  - Print an informative error message when the user supplies a `drake` plan to the `config` argument of a function.
  - By default, use gray arrows and a black-and-white background with no gridlines.
- For the `map()` and `cross()` transformations in the DSL, prevent the accidental sorting of targets by name (#786). Needed `merge(sort = FALSE)` in `dsl_left_outer_join()`.
- Simplify verbosity. The `verbose` argument of `make()` now takes values 0, 1, and 2, and maximum verbosity in the console prints targets, retries, failures, and a spinner. The console log file, on the other hand, dumps maximally verbose runtime info regardless of the `verbose` argument.
- In previous versions, functions generated with `f <- Rcpp::cppFunction(...)` did not stay up to date from session to session because the addresses corresponding to anonymous pointers were showing up in `deparse(f)`. Now, `drake` ignores those pointers, and `Rcpp` functions compiled inline appear to stay up to date. This problem was more of an edge case than a bug.
- Prepend time stamps with sub-second times to the lines of the console log file.
- In `drake_plan()`, deprecate the `tidy_evaluation` argument in favor of the new and more concise `tidy_eval`. To preserve back compatibility for now, if you supply a non-`NULL` value to `tidy_evaluation`, it overwrites `tidy_eval`.
- Reduce the object size of `drake_config()` objects by assigning closure of `config$sleep` to `baseenv()`.

# Version 7.0.0

## Breaking changes

- The enhancements that increase cache access speed also invalidate targets in old projects. Workflows built with drake <= 6.2.1 will need to run from scratch again.
- In `drake` plans, the `command` and `trigger` columns are now lists of language objects instead of character vectors. `make()` and friends still work if you have character columns, but the default output of `drake_plan()` has changed to this new format.
- All parallel backends (`parallelism` argument of `make()`) except "clustermq" and "future" are removed. A new "loop" backend covers local serial execution.
- A large amount of deprecated functionality is now defunct, including several functions (`built()`, `find_project()`, `imported()`, and `parallel_stages()`; full list at #564) and the single-quoted file API.
- Set the default value of `lock_envir` to `TRUE` in `make()` and `drake_config()`. So `make()` will automatically quit in error if the act of building a target tries to change upstream dependencies.
- `make()` no longer returns a value. Users will need to call `drake_config()` separately to get the old return value of `make()`.
- Require the `jobs` argument to be of length 1 (`make()` and `drake_config()`). To parallelize the imports and other preprocessing steps, use `jobs_preprocess`, also of length 1.
- Get rid of the "kernels" `storr` namespace. As a result, `drake` is faster, but users will no longer be able to load imported functions using `loadd()` or `readd()`.
- In `target()`, users must now explicitly name all the arguments except `command`, e.g. `target(f(x), trigger = trigger(condition = TRUE))` instead of `target(f(x), trigger(condition = TRUE))`.
- Fail right away in `bind_plans()` when the result has duplicated target names. This makes `drake`'s API more predictable and helps users catch malformed workflows earlier.
- `loadd()` only loads targets listed in the plan. It no longer loads imports or file hashes.
- The return values of `progress()`, `deps_code()`, `deps_target()`, and `predict_workers()` are now data frames.
- Change the default value of `hover` to `FALSE` in visualization functions. Improves speed.

## Bug fixes

- Allow `bind_plans()` to work with lists of plans (`bind_plans(list(plan1, plan2))` was returning `NULL` in `drake` 6.2.0 and 6.2.1).
- Ensure that `get_cache(path = "non/default/path", search = FALSE)` looks for the cache in `"non/default/path"` instead of `getwd()`.
- Remove strict dependencies on package `tibble`.
- Pass the correct data structure to `ensure_loaded()` in `meta.R` and `triggers.R` when ensuring the dependencies of the `condition` and `change` triggers are loaded.
- Require a `config` argument to `drake_build()` and `loadd(deps = TRUE)`.

## New features

- Introduce a new experimental domain-specific language for generating large plans (#233). Details in the "Plans" chapter of the manual.
- Implement a `lock_envir` argument to safeguard reproducibility. More discussion: #619, #620.
- The new `from_plan()` function allows the users to reference custom plan columns from within commands. Changes to values in these columns columns do not invalidate targets.
- Add a menu prompt (#762) to safeguard against `make()` pitfalls in interactive mode (#761). Appears once per session. Disable with `options(drake_make_menu = FALSE)`.
- Add new API functions `r_make()`, `r_outdated()`, etc. to run `drake` functions more reproducibly in a clean session. See the help file of `r_make()` for details.
- `progress()` gains a `progress` argument for filtering results. For example, `progress(progress = "failed")` will report targets that failed.


## Enhancements

- **Large speed boost**: move away from `storr`'s key mangling in favor of `drake`'s own encoding of file paths and namespaced functions for `storr` keys.
- Exclude symbols `.`, `..`, and `.gitignore` from being target names (consequence of the above).
- Use only one hash algorithm per `drake` cache, which the user can set with the `hash_algorithm` argument of `new_cache()`, `storr::storr_rds()`, and various other cache functions. Thus, the concepts of a "short hash algorithm" and "long hash algorithm" are deprecated, and the functions `long_hash()`, `short_hash()`, `default_long_hash_algo()`, `default_short_hash_algo()`, and `available_hash_algos()` are deprecated. Caches are still back-compatible with `drake` > 5.4.0 and <= 6.2.1.
- Allow the `magrittr` dot symbol to appear in some commands sometimes.
- Deprecate the `fetch_cache` argument in all functions.
- Remove packages `DBI` and `RSQLite` from "Suggests".
- Define a special `config$eval <- new.env(parent = config$envir)` for storing built targets and evaluating commands in the plan. Now, `make()` no longer modifies the user's environment. This move is a long-overdue step toward purity.
- Remove dependency on the `codetools` package.
- Deprecate and remove the `session` argument of `make()` and `drake_config()`. Details: in #623.
- Deprecate the `graph` and `layout` arguments to `make()` and `drake_config()`. The change simplifies the internals, and memoization allows us to do this.
- Warn the user if running `make()` in a subdirectory of the `drake` project root (determined by the location of the `.drake` folder in relation to the working directory).
- In the code analysis, explicitly prohibit targets from being dependencies of imported functions.
- Increase options for the `verbose` argument, including the option to print execution and total build times.
- Separate the building of targets from the processing of imports. Imports are processed with rudimentary staged parallelism (`mclapply()` or `parLapply()`, depending on the operating system).
- Ignore the imports when it comes to build times. Functions `build_times()`, `predict_runtime()`, etc. focus on only the targets.
- Deprecate many API functions, including `plan_analyses()`, `plan_summaries()`, `analysis_wildcard()`, `cache_namespaces()`, `cache_path()`, `check_plan()`, `dataset_wildcard()`, `drake_meta()`, `drake_palette()`, `drake_tip()`, `recover_cache()`, `cleaned_namespaces()`, `target_namespaces()`, `read_drake_config()`, `read_drake_graph()`, and `read_drake_plan()`.
- Deprecate `target()` as a user-side function. From now on, it should only be called from within `drake_plan()`.
- `drake_envir()` now throws an error, not a warning, if called in the incorrect context. Should be called only inside commands in the user's `drake` plan.
- Replace `*expr*()` `rlang` functions with their `*quo*()` counterparts. We still keep `rlang::expr()` in the few places where we know the expressions need to be evaluated in `config$eval`.
- The `prework` argument to `make()` and `drake_config()` can now be an expression (language object) or list of expressions. Character vectors are still acceptable.
- At the end of `make()`, print messages about triggers etc. only if `verbose >= 2L`.
- Deprecate and rename  `in_progress()` to `running()`.
- Deprecate and rename  `knitr_deps()` to `deps_knitr()`.
- Deprecate and rename  `dependency_profile()` to `deps_profile()`.
- Deprecate and rename  `predict_load_balancing()` to `predict_workers()`.
- Deprecate `this_cache()` and defer to `get_cache()` and `storr::storr_rds()` for simplicity.
- Change the default value of `hover` to `FALSE` in visualization functions. Improves speed. Also a breaking change.
- Deprecate `drake_cache_log_file()`. We recommend using `make()` with the `cache_log_file` argument to create the cache log. This way ensures that the log is always up to date with `make()` results.


# Version 6.2.1

Version 6.2.1 is a hotfix to address the failing automated CRAN checks for 6.2.0. Chiefly, in CRAN's Debian R-devel (2018-12-10) check platform, errors of the form "length > 1 in coercion to logical" occurred when either argument to `&&` or `||` was not of length 1 (e.g. `nzchar(letters) && length(letters)`). In addition to fixing these errors, version 6.2.1 also removes a problematic link from the vignette.


# Version 6.2.0

## New features

- Add a `sep` argument to `gather_by()`, `reduce_by()`, `reduce_plan()`, `evaluate_plan()`, `expand_plan()`, `plan_analyses()`, and `plan_summaries()`. Allows the user to set the delimiter for generating new target names.
- Expose a `hasty_build` argument to `make()` and `drake_config()`. Here, the user can set the function that builds targets in "hasty mode" (`make(parallelism = "hasty")`).
- Add a new `drake_envir()` function that returns the environment where `drake` builds targets. Can only be accessed from inside the commands in the workflow plan data frame. The primary use case is to allow users to remove individual targets from memory at predetermined build steps.

## Bug fixes

- Ensure compatibility with `tibble` 2.0.0.
- Stop returning `0s` from `predict_runtime(targets_only = TRUE)` when some targets are outdated and others are not.
- Remove `sort(NULL)` warnings from `create_drake_layout()`. (Affects R-3.3.x.)

## Enhancements

- Remove strict dependencies on packages `evaluate`, `formatR`, `fs`, `future`, `parallel`, `R.utils`, `stats`, and `stringi`.
- **Large speed boost**: reduce repeated calls to `parse()` in `code_dependencies()`.
- **Large speed boost**: change the default value of `memory_strategy` (previously `pruning_strategy`) to `"speed"` (previously `"lookahead"`).
- Compute a special data structure in `drake_config()` (`config$layout`) just to store the code analysis results. This is an intermediate structure between the workflow plan data frame and the graph. It will help clean up the internals in future development.
- Improve memoized preprocessing: deparse all the functions in the environment so the memoization does not react so spurious changes in R internals. Related: #345.
- Use the `label` argument to `future()` inside `make(parallelism = "future")`. That way , job names are target names by default if `job.name` is used correctly in the `batchtools` template file.
- Remove strict dependencies on packages `dplyr`, `evaluate`, `fs`, `future`, `magrittr`, `parallel`, `R.utils`, `stats`, `stringi`, `tidyselect`, and `withr`.
- Remove package `rprojroot` from "Suggests".
- Deprecate the `force` argument in all functions except `make()` and `drake_config()`.
- Change the name of `prune_envir()` to `manage_memory()`.
- Deprecate and rename the `pruning_strategy` argument to `memory_strategy` (`make()` and `drake_config()`).
- Print warnings and messages to the `console_log_file` in real time (#588).
- Use HTML line breaks in `vis_drake_graph()` hover text to display commands in the `drake` plan more elegantly.
- Speed up `predict_load_balancing()` and remove its reliance on internals that will go away in 2019 via #561.
- Remove support for the `worker` column of `config$plan` in `predict_runtime()` and `predict_load_balancing()`. This functionality will go away in 2019 via #561.
- Change the names of the return value of `predict_load_balancing()` to `time` and `workers`.
- Bring the documentation of `predict_runtime()` and `predict_load_balancing()` up to date.
- Deprecate `drake_session()` and rename to `drake_get_session_info()`.
- Deprecate the `timeout` argument in the API of `make()` and `drake_config()`. A value of `timeout` can be still passed to these functions without error, but only the `elapsed` and `cpu` arguments impose actual timeouts now.

# Version 6.1.0

## New features

- **Add a new `map_plan()` function to easily create a workflow plan data frame to execute a function call over a grid of arguments.**
- Add a new `plan_to_code()` function to turn `drake` plans into generic R scripts. New users can use this function to better understand the relationship between plans and code, and unsatisfied customers can use it to disentangle their projects from `drake` altogether. Similarly, `plan_to_notebook()` generates an R notebook from a `drake` plan.
- Add a new `drake_debug()` function to run a target's command in debug mode. Analogous to `drake_build()`.
- Add a `mode` argument to `trigger()` to control how the `condition` trigger factors into the decision to build or skip a target. See the `?trigger` for details.
- Add a new `sleep` argument to `make()` and `drake_config()` to help the master process consume fewer resources during parallel processing.
- Enable the `caching` argument for the `"clustermq"` and `"clustermq_staged"` parallel backends. Now, `make(parallelism = "clustermq", caching = "master")` will do all the caching with the master process, and `make(parallelism = "clustermq", caching = "worker")` will do all the caching with the workers. The same is true for `parallelism = "clustermq_staged"`.
- Add a new `append` argument to `gather_plan()`, `gather_by()`, `reduce_plan()`, and `reduce_by()`. The `append` argument control whether the output includes the original `plan` in addition to the newly generated rows.
- Add new functions `load_main_example()`, `clean_main_example()`, and `clean_mtcars_example()`.
- Add a `filter` argument to `gather_by()` and `reduce_by()` in order to restrict what we gather even when `append` is `TRUE`.
- Add a hasty mode: `make(parallelism = "hasty")` skips all of `drake`'s expensive caching and checking. All targets run every single time and you are responsible for saving results to custom output files, but almost all the by-target overhead is gone.

## Bug fixes

- Ensure commands in the plan are re-analyzed for dependencies when new imports are added (#548). Was a bug in version 6.0.0 only.
- Call `path.expand()` on the `file` argument to `render_drake_graph()` and `render_sankey_drake_graph()`. That way, tildes in file paths no longer interfere with the rendering of static image files.
- Skip tests and examples if the required "Suggests" packages are not installed.
- Stop checking for non-standard columns. Previously, warnings about non-standard columns were incorrectly triggered by `evaluate_plan(trace = TRUE)` followed by `expand_plan()`, `gather_plan()`, `reduce_plan()`, `gather_by()`, or `reduce_by()`. The more relaxed behavior also gives users more options about how to construct and maintain their workflow plan data frames.
- Use checksums in `"future"` parallelism to make sure files travel over network file systems before proceeding to downstream targets.
- Refactor and clean up checksum code.
- Skip more tests and checks if the optional `visNetwork` package is not installed.

## Enhancements

- Stop earlier in `make_targets()` if all the targets are already up to date.
- Improve the documentation of the `seed` argument in `make()` and `drake_config()`.
- Set the default `caching` argument of `make()` and `drake_config()` to `"master"` rather than `"worker"`. The default option should be the lower-overhead option for small workflows. Users have the option to make a different set of tradeoffs for larger workflows.
- Allow the `condition` trigger to evaluate to non-logical values as long as those values can be coerced to logicals.
- Require that the `condition` trigger evaluate to a vector of length 1.
- Keep non-standard columns in `drake_plan_source()`.
- `make(verbose = 4)` now prints to the console when a target is stored.
- `gather_by()` and `reduce_by()` now gather/reduce everything if no columns are specified.
- Change the default parallelization of the imports. Previously, `make(jobs = 4)` was equivalent to `make(jobs = c(imports = 4, targets = 4))`. Now, `make(jobs = 4)` is equivalent to `make(jobs = c(imports = 1, targets = 4))`. See issue #553 for details.
- Add a console message for building the priority queue when `verbose` is at least 2.
- Condense `load_mtcars_example()`.
- Deprecate the `hook` argument of `make()` and `drake_config()`.
- In `gather_by()` and `reduce_by()`, do not exclude targets with all `NA` gathering variables.

# Version 6.0.0

## Breaking changes

- Avoid serialization in `digest()` wherever possible. This puts old `drake` projects out of date, but it improves speed.
- Require R version >= 3.3.0 rather than >= 3.2.0. Tests and checks still run fine on 3.3.0, but the required version of the `stringi` package no longer compiles on 3.2.0.
- Be more discerning in detecting dependencies. In `code_dependencies()`, restrict the possible global variables to the ones mentioned in the new `globals` argument (turned off when `NULL`. In practical workflows, global dependencies are restricted to items in `envir` and proper targets in the plan. In `deps_code()`, the `globals` slot of the output list is now a list of *candidate* globals, not necessarily actual globals (some may not be targets or variables in `envir`).

## Bug fixes

- In the call to `unlink()` in `clean()`, set `recursive` and `force` to `FALSE`. This should prevent the accidental deletion of whole directories.
- Previously, `clean()` deleted input-only files if no targets from the plan were cached. A patch and a unit test are included in this release.
- `loadd(not_a_target)` no longer loads every target in the cache.
- Exclude each target from its own dependency metadata in the "deps" `igraph` vertex attribute (fixes #503).
- Detect inline code dependencies in `knitr_in()` file code chunks.
- Remove more calls to `sort(NULL)` that caused warnings in R 3.3.3.
- Fix a bug on R 3.3.3 where `analyze_loadd()` was sometimes quitting with "Error: attempt to set an attribute on NULL".
- Do not call `digest::digest(file = TRUE)` on directories. Instead, set hashes of directories to `NA`. Users should still not directories as file dependencies.
- If files are declared as dependencies of custom triggers ("condition" and "change") include them in `vis_drake_graph()`. Previously, these files were missing from the visualization, but actual workflows worked just fine.
- Work around mysterious `codetools` failures in R 3.3 (add a `tryCatch()` statement in `find_globals()`).

## New features

- Add a proper `clustermq`-based parallel backend: `make(parallelism = "clustermq")`.
- `evaluate_plan(trace = TRUE)` now adds a `*_from` column to show the origins of the evaluated targets. Try `evaluate_plan(drake_plan(x = rnorm(n__), y = rexp(n__)), wildcard = "n__", values = 1:2, trace = TRUE)`.
- Add functions `gather_by()` and `reduce_by()`, which gather on custom columns in the plan (or columns generated by `evaluate_plan(trace = TRUE)`) and append the new targets to the previous plan.
- Expose the `template` argument of `clustermq` functions (e.g. `Q()` and `workers()`) as an argument of `make()` and `drake_config()`.
- Add a new `code_to_plan()` function to turn R scripts and R Markdown reports into workflow plan data frames.
- Add a new `drake_plan_source()` function, which generates lines of code for a `drake_plan()` call. This `drake_plan()` call produces the plan passed to `drake_plan_source()`. The main purpose is visual inspection (we even have syntax highlighting via `prettycode`) but users may also save the output to a script file for the sake of reproducibility or simple reference.
- Deprecate `deps_targets()` in favor of a new `deps_target()` function (singular) that behaves more like `deps_code()`.

## Enhancements

- Smooth the edges in `vis_drake_graph()` and `render_drake_graph()`.
- Make hover text slightly more readable in in `vis_drake_graph()` and `render_drake_graph()`.
- Align hover text properly in `vis_drake_graph()` using the "title" node column.
- Optionally collapse nodes into clusters with `vis_drake_graph(collapse = TRUE)`.
- Improve `dependency_profile()` show major trigger hashes side-by-side
to tell the user if the command, a dependency, an input file, or an output file changed since the last `make()`.
- Choose more appropriate places to check that the `txtq` package is installed.
- Improve the help files of `loadd()` and `readd()`, giving specific usage guidance in prose.
- Memoize all the steps of `build_drake_graph()` and print to the console the ones that execute.
- Skip some tests if `txtq` is not installed.

# Version 5.4.0

- Overhaul the interface for triggers and add new trigger types ("condition" and "change").
- Offload `drake`'s code examples to the `drake-examples` GitHub repository and make make `drake_example()` and `drake_examples()` download examples from there.
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
- Implement `make(parallelism = "clustermq_staged")`, a `clustermq`-based staged parallelism backend (see #452).
- Implement `make(parallelism = "future_lapply_staged")`, a `future`-based staged parallelism backend (see #450).
- Depend on `codetools` rather than `CodeDepends` for finding global variables.
- Detect `loadd()` and `readd()` dependencies in `knitr` reports referenced with `knitr_in()` inside imported functions. Previously, this feature was only available in explicit `knitr_in()` calls in commands.
- Skip more tests on CRAN. White-list tests instead of blacklisting them in order to try to keep check time under the official 10-minute cap.
- Disallow wildcard names to grep-match other wildcard names or any replacement values. This will prevent careless mistakes and confusion when generating `drake_plan()`s.
- Prevent persistent workers from hanging when a target fails.
- Move the example template files to `inst/hpc_template_files`.
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
- Remove some remnants of staged parallelism internals.
- Allow different parallel backends for imports vs targets. For example, `make(parallelism = c(imports = "mclapply_staged", targets = "mclapply")`.
- Fix a bug in environment pruning. Previously, dependencies of downstream targets were being dropped from memory in `make(jobs = 1)`. Now, they are kept in memory until no downstream target needs them (for `make(jobs = 1)`).
- Improve `predict_runtime()`. It is a more sensible way to go about predicting runtimes with multiple jobs. Likely to be more accurate.
- Calls to `make()` no longer leave targets in the user's environment.
- Attempt to fix a Solaris CRAN check error. A test was previously failing on CRAN's Solaris machine (R 3.5.0). In the test, one of the threads deliberately quits in error, and the R/Solaris installation did not handle this properly. The test should work now because it no longer uses any parallelism.
- Deprecate the `imports_only` argument to `make()` and `drake_config()` in favor of `skip_targets`.
- Deprecate `migrate_drake_project()`.
- Deprecate `max_useful_jobs()`.
- For non-distributed parallel backends, stop waiting for all the imports to finish before the targets begin.
- Add an `upstream_only` argument to `failed()` so users can list failed targets that do not have any failed dependencies. Naturally accompanies `make(keep_going = TRUE)`.
- Add an RStudio R Markdown template.
- Remove `plyr` as a dependency.
- Handle duplicated targets better in `drake_plan()` and `bind_plans()`.
- Add a true function `target()` to help create drake plans with custom columns.
- In `drake_gc()`, clean out disruptive files in `storr`s with mangled keys (re: #198).
- Move all the vignettes to the up and coming user manual.
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
- Forcibly exclude the dot (`.`) from being a dependency of any target or import. This enforces more consistent behavior in the face of the current static code analysis functionality, which sometimes detects `.` and sometimes does not.
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
- Add a `replace` argument to `loadd()` so that objects already in the user's environment need not be replaced.
- When the graph cyclic, print out all the cycles.
- Prune self-referential loops (and duplicate edges) from the workflow graph. That way, recursive functions are allowed.
- Add a `seed` argument to `make()`, `drake_config()`, and `load_basic_example()`. Also hard-code a default seed of `0`. That way, the pseudo-randomness in projects should be reproducible
across R sessions.
- Cache the pseudo-random seed at the time the project is created and use that seed to build targets until the cache is destroyed.
- Add a new `drake_read_seed()` function to read the seed from the cache. Its examples illustrate what `drake` is doing to try to ensure reproducible random numbers.
- Evaluate the quasiquotation operator `!!` for the `...` argument to `drake_plan()`. Suppress this behavior using `tidy_evaluation = FALSE` or by passing in commands passed through the `list` argument.
- Preprocess workflow plan commands with `rlang::expr()` before evaluating them. That means you can use the quasiquotation operator `!!` in your commands, and `make()` will evaluate them according to the tidy evaluation paradigm.
- Restructure `drake_example("basic")`, `drake_example("gsp")`, and `drake_example("packages")` to demonstrate how to set up the files for serious `drake` projects. More guidance was needed in light of #193.
- Improve the examples of `drake_plan()` in the help file (`?drake_plan`).

# Version 5.0.0

- Transfer `drake` to rOpenSci GitHub URL.
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
- Add `"future_lapply"` parallelism: parallel backends supported by the `future` and `future.batchtools` packages. See `?backend` for examples and the parallelism vignette for an introductory tutorial. More advanced instruction can be found in the `future` and `future.batchtools` packages themselves.
- Cache diagnostic information of targets that fail and retrieve diagnostic info with `diagnose()`.
- Add an optional `hook` argument to `make()` to wrap around `build()`. That way, users can more easily control the side effects of distributed jobs. For example, to redirect error messages to a file in `make(..., parallelism = "Makefile", jobs = 2, hook = my_hook)`, `my_hook` should be something like `function(code){withr::with_message_sink("messages.txt", code)}`.
- Remove console logging for "parLapply" parallelism. `drake` was previously using the `outfile` argument for PSOCK clusters to generate output that could not be caught by `capture.output()`. It was a hack that should have been removed before.
- Remove console logging for "parLapply" parallelism. `drake` was previously using the `outfile` argument for PSOCK clusters to generate output that could not be caught by `capture.output()`. It was a hack that should have been removed before.
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
- Reproducible random numbers (#56)
- Automatic detection of knitr dependencies (#9)
- More vignettes
- Bug fixes

# Version 4.2.0: 2017-09-29

Version 4.2.0 will be released today. There are several improvements to code style and performance. In addition, there are new features such as cache/hash externalization and runtime prediction. See the new storage and timing vignettes for details. This release has automated checks for back-compatibility with existing projects, and I also did manual back compatibility checks on serious projects.

# Version 3.0.0: 2017-05-03

Version 3.0.0 is coming out. It manages environments more intelligently so that the behavior of `make()` is more consistent with evaluating your code in an interactive session.

# Version 1.0.1: 2017-02-28

Version 1.0.1 is on CRAN! I'm already working on a massive update, though. 2.0.0 is cleaner and more powerful.

