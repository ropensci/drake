- carefully proofread files in basically this order
  4. cache.R
  5. clean.R
  6. run.R
  7. DESCRIPTION
troubleshooting.md
cran-comments.md
logo.png
- implement check()
- all tests
- vignettes
  1. thorough manual: drake_manual.Rmd
  2. extended example: drake_example.Rmd
    - bake in this example using inst/ and example_drake() and examples_drake()
- deprecation
  - run(), not make()
  - target/command, not output/code
