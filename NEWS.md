# Changes in release 4.4.0

- If 'verbose' is 'TRUE' and all targets are already up to date (nothing to build), then `make()` and `outdated()` print "All targets are already up to date" to the console.
- New support for timeouts and retries when it comes to building targets.
- Failed targets are now recorded during the build process. You can see them in `plot_graph()` and `progress()`. Also see the new `failed()` function, which is similar to `in_progress()`.
- Speed up the overhead of `parLapply` parallelism. The downside to this fix is that `drake` has to be properly installed. It should not be loaded with `devtools::load_all()`. The speedup comes from lightening the first `clusterExport()` call in `run_parLapply()`. Previously, we exported every single individual `drake` function to all the workers, which created a bottleneck. Now, we just load `drake` itself in each of the workers, which works because `build()` and `do_prework()` are exported. 

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
