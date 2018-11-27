extract_filenames <- function(command) {
  if (!safe_grepl("'", command, fixed = TRUE)) {
    return(character(0))
  }
  splits <- paste(" ", command, " ")
  splits <- strsplit(splits, split = "'")
  splits <- unlist(splits)
  splits[seq(from = 2, to = length(splits), by = 2)]
}

# Get the command ready for tidy eval prep
# and then pure eval (no side effects).
preprocess_command <- function(command, config) {
  if (is.character(command)){
    command <- parse(text = command)
  }
  command <- as.call(c(quote(`{`), command))
  command <- as.call(c(quote(local), command))
  as.call(c(quote(rlang::expr), command))
}

# Can remove once we remove fetch_cache.
# We can remove fetch_cache once we allow the master process
# to optionally do all the caching.
localize <- function(command) {
  paste0("local({\n", command, "\n})")
}

standardize_command <- function(x) {
  x <- ignore_ignore(x)
  x <- language_to_text(x)
  standardize_code(x)
}

# We won't need this function after #563.
language_to_text <- function(x) {
  if (length(x) < 1) {
    return(character(0)) # nocov
  }
  if (is.expression(x)) {
    # TODO: remove the if () clause in some major version bump.
    # The only reason it exists is to avoid invalidating old projects.
    if (length(x) < 2) {
      x <- x[[1]]
    }
  }
  if (is.expression(x) || is.language(x)) {
    for (attribute in c("srcref", "srcfile", "wholeSrcref")) {
      attr(x = x, which = attribute) <- NULL
    }
    x <- wide_deparse(x)
  }
  x
}

standardize_code <- function(x){
  if (!length(x)){
    return(as.character(NA))
  }
  x <- deparse(parse(text = as.character(x), keep.source = FALSE)[[1]])
  x <- paste(x, collapse = "\n")
  expr <- parse(text = x, keep.source = TRUE)
  info <- utils::getParseData(expr, includeText = TRUE)
  change <- info$token == "LEFT_ASSIGN"
  info$text[change] <- "="
  info$token[change] <- "EQ_ASSIGN"
  out <- info[info$token != "COMMENT" & info$terminal, c("token", "text")]
  out <- lapply(out, FUN = paste, collapse = " ")
  out <- paste(out, collapse = " >> ")
  digest::digest(out, algo = "sha256", serialize = FALSE)
}
