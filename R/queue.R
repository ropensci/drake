Queue <- R6::R6Class(
  classname = "Queue",
  private = list(
    items = character(0)
  ),
  public = list(
    initialize = function(items = character(0)){
      stopifnot(is.character(items))
      private$items <- items
    },
    size = function(){
      length(private$items)
    },
    empty = function(){
      self$size() < 1
    },
    push = function(item){
      private$items[self$size() + 1] <- item
    },
    pop = function(n = 1){
      if (self$empty() || n < 1){
        return(character(0))
      }
      n <- min(n, self$size())
      index_pop <- seq_len(n)
      out <- private$items[index_pop]
      private$items <- private$items[-index_pop]
      out
    },
    peek = function(n = 1){
      if (self$empty() || n < 1){
        return(character(0))
      }
      n <- min(n, self$size())
      private$items[seq_len(n)]
    },
    list = function(){
      self$peek(n = self$size())
    }
  )
)
