parsable_list <- function(x){
  lapply(x, function(y) Filter(is_parsable, y))
}

is_parsable <- Vectorize(function(x){
  tryCatch({
      parse(text = x)
      TRUE
    },
    error = error_false
  )
},
"x")

extract_filenames <- function(command){
  if (!safe_grepl("'", command)){
    return(character(0))
  }
  splits <- str_split(command, "'")[[1]]
  splits[seq(from = 2, to = length(splits), by = 2)]
}

# This is the version of the command that is
# actually run in make(), not the version
# that is cached and treated as a dependency.
# It needs to (1) wrap the command in a function
# to protect the user's environment from side effects,
# and (2) call rlang::expr() to enable tidy evaluation
# features such as quasiquotation.
preprocess_command <- function(target, config){
  text <- config$plan$command[config$plan$target == target] %>%
    wrap_command
  parse(text = text, keep.source = FALSE) %>%
    eval(envir = config$envir)
}

# Use tidy evaluation to complete the contents of a command.
wrap_command <- function(command){
  paste0("rlang::expr(local({\n", command, "\n}))")
}

# Can remove once we remove fetch_cache.
# We can remove fetch_cache once we allow the master process
# to optionally do all the caching.
localize <- function(command) {
  paste0("local({\n", command, "\n})")
}

# This version of the command will be hashed and cached
# as a dependency. When the command changes nontrivially,
# drake will react. Otherwise, changes to whitespace or
# comments are just standardized away, and drake
# ignores them. Thus, superfluous builds are not triggered.
get_standardized_command <- function(target, config) {
  config$plan$command[config$plan$target == target] %>%
    standardize_command
}

# The old standardization command
# that relies on formatR.
# Eventually, we may move to styler,
# since it is now the preferred option for
# text tidying.
# The important thing for drake's standardization of commands
# is to stay stable here, not to be super correct.
# If styler's behavior changes a lot, it will
# put targets out of date.
standardize_command <- function(x) {
  formatR::tidy_source(
    source = NULL,
    comment = FALSE,
    blank = FALSE,
    arrow = TRUE,
    brace.newline = FALSE,
    indent = 4,
    output = FALSE,
    text = x,
    width.cutoff = 119
  )$text.tidy %>%
    paste(collapse = "\n") %>%
    braces
}
