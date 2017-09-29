# Version 4.2.0

There are several improvements to code style and performance. In addition, there are new features such as cache/hash externalization and runtime prediction. See the new storage and timing vignettes for details. This release has automated checks for back-compatibility with existing projects, and I also did manual back compatibility checks on serious projects.

# Version 4.0.0

This major release is almost back-compatible. It now reproducibly tracks namespaced functions of the form `package::function()` and `package:::function()`. Adding this behavior was the right thing to do, but some projects from <= 3.0.0 might not be back-compatible. 

This release also includes a round of bugfixes, better parallel computing, completely rewritten documentation, and new functionality to enhance the user experience. `plot_graph()` now uses `visNetwork` in the backend, and the interactive visualization helps users understand the state of thier workflows. New functions also include `load_basic_example()`, `dataframes_graph()`, `max_useful_jobs()`, `deps()`, `outdated()`, and `shell_file()`.

# Version 3.0.0

This major version update contains crucial bug fixes and documentation updates. It improves the way environments are managed so the behavior of `make()` is more predictable. The interface is completely back-compatible, but the behavior of the internals is somewhat different, which is why I bumped to a new major version rather than a new minor version or patch.

The biggest changes are that `config()` does not create a deep copy of the `envir` argument of `make()` and that the enclosing environments of imported functions are left alone. The upshot for most projects is that the user's workspace is modified, but behavior is more predictable and more consistent with execution in an R session outside of drake.

# Version 2.1.0

This version fixes a vignette build error on Solaris, touches up the logo, and adds parLapply() to the arsenal of parallel computing options (now the default). I also added the `tracked()` function to list which objects, functions, files, targets, etc. are reproducibly tracked. Lastly, there is an important new "caution" vignette. It tells users about the edge cases that can trick drake into overlooking dependencies, as well as ways to verify which objects are being reproducibly tracked.

# Version 2.0.0

This version is not back-compatible with version 1.0.1.
I still tried my best to deprecate the user-level functions, but because
of all the internal updates, existing workflows will need to rerun
from scratch due to the upgrade.

# Version 1.0.1

This version is the first submission to CRAN.
