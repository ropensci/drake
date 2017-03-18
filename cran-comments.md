# Version 2.1.0

This version fixes a vignette build error on Solaris, touches up the logo, and adds parLapply() to the arsenal of parallel computing options (now the default). I also added the `tracked()` function to list which objects, functions, files, targets, etc. are reproducibly tracked. Lastly, there is an important new "caution" vignette. It tells users about the edge cases that can trick drake into overlooking dependencies, as well as ways to verify which objects are being reproducibly tracked.

# Version 2.0.0

This version is not back-compatible with version 1.0.1.
I still tried my best to deprecate the user-level functions, but because
of all the internal updates, existing workflows will need to rerun
from scratch due to the upgrade.

# Version 1.0.1

This version is the first submission to CRAN.
