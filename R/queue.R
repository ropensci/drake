new_target_queue <- function(config){
  config$graph <- config$schedule
  targets <- V(config$graph)$name
  if (!length(targets)){
    return(R6_priority_queue$new())
  }
  ndeps <- lightly_parallelize(
    X = targets,
    FUN = function(target){
      length(dependencies(targets = target, config = config))
    },
    jobs = config$jobs
  ) %>%
    unlist
  if ("priority" %in% colnames(config$plan)){
    priorities <- config$plan$priority
  } else {
    priorities <- rep(0, length(targets))
  }
  R6_priority_queue$new(
    targets = targets,
    ndeps = ndeps,
    priorities = priorities
  )
}

# This is not actually a serious O(log n) priority queue
# based on a binary heap. It is a naive placeholder.
# The real priority queue will be
# https://github.com/dirmeier/datastructures
# once the CRAN version has decrease-key
# (https://github.com/dirmeier/datastructures/issues/4).
R6_priority_queue <- R6::R6Class(
  classname = "R6_priority_queue",
  public = list(
    data = data.frame(
      targets = character(0),
      ndeps = integer(0),
      priorities = numeric(0)
    ),
    initialize = function(
      targets = character(0),
      ndeps = integer(0),
      priorities = numeric(0)
    ){
      stopifnot(length(targets) == length(ndeps))
      self$data <- data.frame(
        target = targets,
        ndeps = ndeps,
        priority = priorities,
        stringsAsFactors = FALSE
      )
      self$sort()
    },
    size = function(){
      nrow(self$data)
    },
    empty = function(){
      self$size() < 1
    },
    sort = function(){
      ndeps <- priority <- NULL
      self$data <- dplyr::arrange(self$data, ndeps, priority)
    },
    # Peek at the head node of the queue
    # if and only if its ndeps is 0.
    peek0 = function(){
      if (!self$empty() && self$data$ndeps[1] < 1){
        self$data$target[1]
      }
    },
    # Extract the head node of the queue
    # if and only if its ndeps is 0.
    pop0 = function(){
      if (!self$empty() && self$data$ndeps[1] < 1){
        out <- self$data$target[1]
        self$data <- self$data[-1, ]
        out
      }
    },
    # This is all wrong and inefficient.
    # Needs the actual decrease-key algorithm
    decrease_key = function(targets){
      index <- self$data$target %in% targets
      self$data$ndeps[index] <- self$data$ndeps[index] - 1
      self$sort()
    }
  )
)
