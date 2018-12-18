codetools_tmp_str <- "*ct_tmp*"
codetools_tmpv_str <- "*ct_tmpv*"
codetools_tmp <- as.name(codetools_tmp_str)
codetools_tmpv <- as.name(codetools_tmpv_str)
drake_prefix <- c("", "drake::", "drake:::")
drake_envir_marker <- "._drake_envir"
file_in_fns <- pair_text(drake_prefix, c("file_in"))
file_out_fns <- pair_text(drake_prefix, c("file_out"))
ignored_fns <- pair_text(drake_prefix, c("drake_envir", "ignore"))
knitr_in_fns <- pair_text(drake_prefix, c("knitr_in"))
loadd_fns <- pair_text(drake_prefix, "loadd")
readd_fns <- pair_text(drake_prefix, "readd")
target_fns <- pair_text(drake_prefix, "target")
trigger_fns <- pair_text(drake_prefix, "trigger")

drake_symbols <- sort(
  c(
    codetools_tmp_str,
    codetools_tmpv_str,
    drake_envir_marker,
    file_in_fns,
    file_out_fns,
    ignored_fns,
    loadd_fns,
    knitr_in_fns,
    readd_fns,
    target_fns,
    trigger_fns
  )
)

base_symbols <- sort(
  grep(
    pattern = "^[\\.a-zA-Z]",
    x = ls("package:base"),
    value = TRUE,
    invert = TRUE
  )
)

ignored_symbols <- sort(c(drake_symbols, base_symbols))
