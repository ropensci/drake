R6_message_queue <- R6::R6Class(
  classname = "R6_message_queue",
  private = list(
    mq_exclusive = function(code){
      on.exit(filelock::unlock(x))
      x <- filelock::lock(self$lock)
      force(code)
    },
    mq_get_head = function(){
      scan(self$head, quiet = TRUE, what = integer())
    },
    mq_set_head = function(n){
      write(x = as.integer(n), file = self$head, append = FALSE)
    },
    mq_count = function(){
      as.integer(
        R.utils::countLines(self$db) - private$mq_get_head() + 1
      )
    },
    mq_pop = function(){
      out <- private$mq_list(1)
      if (!nrow(out)){
        return(out)
      }
      new_head <- private$mq_get_head() + 1
      private$mq_set_head(new_head)
      out
    },
    mq_push = function(title, message){
      stopifnot(length(title) == 1 && length(message) == 1)
      out <- data.frame(
        title = base64url::base64_urlencode(as.character(title)),
        message = base64url::base64_urlencode(as.character(message)),
        stringsAsFactors = FALSE
      )
      write.table(
        out,
        file = self$db,
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE,
        sep = "|",
        quote = FALSE
      )
    },
    mq_list = function(n){
      if (private$mq_count() < 1){
        return(
          data.frame(
            title = character(0),
            message = character(0),
            stringsAsFactors = FALSE
          )
        )
      }
      out <- read.table(
        self$db, 
        sep = "|",
        skip = private$mq_get_head() - 1,
        nrows = n,
        stringsAsFactors = FALSE,
        header = FALSE,
        quote = ""
      )
      colnames(out) <- c("title", "message")
      out$title <- base64url::base64_urldecode(out$title)
      out$message <- base64url::base64_urldecode(out$message)
      out
    }
  ),
  public = list(
    path = character(0),
    db = character(0),
    head = character(0),
    lock = character(0),
    initialize = function(path){
      self$path <- fs::dir_create(path)
      self$lock <- file.path(self$path, "lock")
      private$mq_exclusive({
        self$db <- fs::file_create(file.path(self$path, "db"))
        self$head <- fs::file_create(file.path(self$path, "head"))
        if (length(private$mq_get_head()) < 1){
          private$mq_set_head(1)
        }
        fs::file_chmod(self$path, mode = "777")
        fs::file_chmod(self$db, mode = "777")
        fs::file_chmod(self$head, mode = "777")
      })
    },
    count = function(){
      private$mq_exclusive(private$mq_count())
    },
    empty = function(){
      self$count() < 1
    },
    list = function(n = -1){
      private$mq_exclusive(private$mq_list(n = n))
    },
    pop = function(){
      private$mq_exclusive(private$mq_pop())
    },
    push = function(title, message){
      private$mq_exclusive(private$mq_push(title = title, message = message))
    },
    destroy = function(){
      unlink(self$path, recursive = TRUE, force = TRUE)
    }
  )
)
