- carefully proofread files in basically this order
  4. cache.R
  5. clean.R
  6. run.R

- docs (after tests)
  1. vignettes/drake_manual.Rmd
  2. vignettes/drake_example.Rmd
  3. slides

- normal tests (1 process, 2 process, Makefiles)
  - check(): check_strings(), etc.
  - run(): everything built first time 
  - rerun builds nothing
  - imported file changes
  - imported file missing: make(), check()
  - command changes trivially
  - command changes but gives the same answer
  - command changes and gives a different answer
  - nested imported function changes trivially
  - nested imported function changes but gives the same answer
  - nested imported function changes and gives a different answer
  - nested imported object changes
  - intermediate file rewritten but not changed
  - intermediate file rewritten and changed
  - intermediate file deleted
- test misc functionality
  - partial collections of targets can be made
  - rehashing happens as expected
- cache functions
  - cached()
  - built()
  - imported()
  - readd: character_only, path, search
  - loadd: ..., list() (and quoting in lots of ways), imported_only, path, search
  - get_cache, find_project, session(): path, search
- clean and prune
  - clean: destroy = TRUE, FALSE. check that uncache removes target from all relevant namespaces (objects, depends, maybe more. check all the namespaces i have, including with makefiles).
  - prune, just in debug plan. then check if intermediate file is removed.
- test edge cases
  - add new target that conflicts with a previous import
  - add new target that conflicts with a current import
- deprecated
  - deprecated make(): deprecation + it still works
  - old output/code names are corrected and then still work in both check() and make()

- on my own
  - makefiles: tests/makefiles
  - jady: be sure to test packages/prework
