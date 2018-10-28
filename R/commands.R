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
  if (!safe_grepl("'", command, fixed = TRUE)){
    return(character(0))
  }
  splits <- paste(" ", command, " ") %>%
    strsplit("'") %>%
    unlist()
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

standardize_command <- function(x) {
  ignore_ignore(x) %>%
    language_to_text() %>%
    standardize_code()
}

language_to_text <- function(x){
  if (length(x) < 1){
    return(character(0))
  }
  if (is.expression(x)){
    # TODO: remove the if () clause in some major version bump.
    # The only reason it exists is to avoid invalidating old projects.
    if (length(x) < 2){
      x <- x[[1]]
    }
  }
  if (is.expression(x) || is.language(x)){
    for (attribute in c("srcref", "srcfile", "wholeSrcref")){
      attr(x = x, which = attribute) <- NULL
    }
    x <- wide_deparse(x)
  }
  x
}

standardize_code <- function(x){
  x <- as.character(x)
  if (!length(x)){
    return(x)
  }
  parsed <- parse(text = x, keep.source = TRUE)
  info <- utils::getParseData(parsed, includeText = TRUE)
  lines <- replace_equals(lines = strsplit(x, split = "\n")[[1]], info = info)
  out <- parse(
    text = paste0(lines, collapse = "\n"),
    keep.source = FALSE
  )[[1]]
  paste0(deparse(out), collapse = "\n")
}

replace_equals <- function(lines, info){
  if (is.null(info)){
    return(lines)
  }
  equals <- info[info$token == "EQ_ASSIGN", ]
  for (line in unique(equals$line1)){
    line_info <- equals[equals$line1 == line, ]
    for (col in sort(line_info$col1, decreasing = TRUE)){
      lines[line] <- paste0(
        substr(x = lines[line], start = 0, stop = col - 1),
        "<-",
        substr(x = lines[line], start = col + 1, stop = nchar(lines[line]))
      )
    }
  }
  lines
}
