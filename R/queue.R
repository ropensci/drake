new_target_queue <- function(config){
  config$graph <- config$schedule
  targets <- V(config$graph)$name
  priorities <- lightly_parallelize(
    X = targets,
    FUN = function(target){
      length(dependencies(targets = target, config = config))
    },
    jobs = config$jobs
  ) %>%
    unlist
  stopifnot(any(priorities < 1)) # Stop if nothing has ready deps.
  R6_priority_queue$new(names = targets, priorities = priorities)
}

# This is not actually a serious O(log n) priority queue
# based on a binary heap. It is a naive placeholder.
# The real priority queue will be
# https://github.com/dirmeier/datastructures
# once the CRAN version has decrease-key
# (https://github.com/dirmeier/datastructures/issues/4).
R6_priority_queue <- R6::R6Class(
  classname = "R6_priority_queue",
  private = list(
    data = numeric(0),
    return_value = function(x, what = c("names", "priorities")){
      what <- match.arg(what)
      if (what == "names"){
        names(x)
      } else {
        x
      }
    }
  ),
  public = list(
    # The values are priorities and the names are targets.
    initialize = function(names = character(0), priorities = numeric(0)){
      self$push(names = names, priorities = priorities)
    },
    size = function(){
      length(private$data)
    },
    empty = function(){
      self$size() < 1
    },
    list = function(what = "names"){
      self$peek(n = self$size(), what = what)
    },
    # Are any targets ready to build?
    # the priority is the number of dependencies left to build.
    any0 = function(){
      !self$empty() && private$data[1] < 1
    },
    sort = function(){
      private$data <- sort(private$data)
    },
    push = function(names = character(0), priorities = numeric(0)){
      stopifnot(length(names) == length(priorities))
      if (!length(names)){
        return()
      }
      data <- priorities
      names(data) <- names
      stopifnot(!length(data) || !is.null(names(data)))
      private$data <- c(private$data, data)
      self$sort()
    },
    peek = function(n = 1, what = "names"){
      if (self$empty() || n < 1){
        return(character(0))
      }
      x <- private$data[seq_len(min(n, self$size()))]
      private$return_value(x = x, what = what)
    },
    pop = function(n = 1, what = "names"){
      if (n < 1){
        return(character(0))
      }
      index <- seq_len(min(n, self$size()))
      x <- private$data[index]
      private$data <- private$data[-index]
      private$return_value(x = x, what = what)
    },
    # Extract the head node of the queue
    # if and only if its priority is 0.
    pop0 = function(what = "names"){
      if (!self$empty() && private$data[1] < 1){
        self$pop(n = 1, what = what)
      }
    },
    # This is all wrong and inefficient.
    # Needs the actual decrease-key algorithm
    decrease_key = function(names){
      private$data[names] <- private$data[names] - 1
      self$sort()
    }
  )
)
