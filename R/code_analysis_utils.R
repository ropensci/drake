is_callish <- function(x) {
  length(x) > 0 && is.language(x) && (is.call(x) || is.recursive(x))
}

is_target_call <- function(expr) {
  tryCatch(
    wide_deparse(expr[[1]]) %in% target_fns,
    error = error_false
  )
}

is_trigger_call <- function(expr) {
  tryCatch(
    wide_deparse(expr[[1]]) %in% trigger_fns,
    error = error_false
  )
}

list_code_analysis_results <- function(results) {
  x <- lapply(
    X = code_analysis_slots,
    FUN = function(slot) {
      ht_list(results[[slot]])
    }
  )
  names(x) <- code_analysis_slots
  select_nonempty(x)
}

new_code_analysis_results <- function() {
  x <- lapply(
    X = code_analysis_slots,
    FUN = function(tmp) {
      ht_new()
    }
  )
  names(x) <- code_analysis_slots
  list2env(x = x, hash = TRUE, parent = emptyenv())
}

pair_text <- function(x, y) {
  apply(expand.grid(x, y), 1, paste0, collapse = "")
}

safe_all_vars <- function(expr) {
  out <- lapply(expr, all.vars)
  as.character(unlist(out))
}

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

code_analysis_slots <- c(
  "globals",
  "namespaced",
  "strings",
  "loadd",
  "readd",
  "file_in",
  "file_out",
  "knitr_in"
)

knitr_in_slots <- c(
  "knitr_in",
  "file_in",
  "file_out",
  "loadd",
  "readd"
)
