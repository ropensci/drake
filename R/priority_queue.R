priority_queue <- function(config, jobs = config$jobs_preprocess) {
  config$logger$minor("construct priority queue")
  targets <- igraph::V(config$graph)$name
  if (!length(targets)) {
    return(
      refclass_priority_queue$new(
        data = data.frame(
          target = character(0),
          ndeps = integer(0),
          priority = numeric(0),
          stringsAsFactors = FALSE
        )
      )
    )
  }
  ndeps <- igraph::degree(graph = config$graph, v = targets, mode = "in")
  ndeps <- unlist(ndeps)
  priority <- rep(Inf, length(targets)) # deprecated, 2019-04-16
  names(priority) <- targets
  queue <- refclass_priority_queue$new(
    data = data.frame(
      target = as.character(targets),
      ndeps = as.integer(ndeps),
      priority = as.numeric(priority),
      stringsAsFactors = FALSE
    )
  )
  queue$sort()
  queue
}

# This is not actually a serious O(log n) priority queue
# based on a binary heap. It is a naive placeholder.
# I we can drop down to C if we need something faster.
refclass_priority_queue <- methods::setRefClass(
  Class = "refclass_priority_queue",
  fields = list(data = "data.frame"),
  methods = list(
    size = function() {
      nrow(.self$data)
    },
    empty = function() {
      .self$size() < 1
    },
    list = function() {
      .self$data$target
    },
    sort = function() {
      ndeps <- priority <- NULL
      precedence <- with(.self$data, order(ndeps, priority))
      .self$data <- .self$data[precedence, ]
    },
    # Peek at the head node of the queue
    # if and only if its ndeps is 0.
    peek0 = function() {
      if (!.self$empty() && .self$data$ndeps[1] < 1) {
        .self$data$target[1]
      }
    },
    # Extract the head node of the queue
    # if and only if its ndeps is 0.
    pop0 = function() {
      if (!.self$empty() && .self$data$ndeps[1] < 1) {
        out <- .self$data$target[1]
        .self$data <- .self$data[-1, ]
        out
      }
    },
    # Get all the ready targets
    list0 = function() {
      if (!.self$empty() && .self$data$ndeps[1] < 1) {
        .self$data$target[.self$data$ndeps < 1]
      }
    },
    remove = function(targets) {
      .self$data <- .self$data[!(.self$data$target %in% targets), ]
      invisible()
    },
    # This is all wrong and inefficient.
    # Needs the actual decrease-key algorithm
    decrease_key = function(targets) {
      index <- .self$data$target %in% targets
      .self$data$ndeps[index] <- .self$data$ndeps[index] - 1
      .self$sort()
    }
  )
)

# Very specific to drake, does not belong inside
# a generic priority queue.
decrease_revdep_keys <- function(queue, target, config) {
  revdeps <- deps_graph(
    targets = target,
    graph = config$graph,
    reverse = TRUE
  )
  revdeps <- intersect(revdeps, queue$list())
  queue$decrease_key(targets = revdeps)
}
