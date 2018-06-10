message_queue <- function(path, create){
  R6_message_queue$new(path = path, create = create)
}

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
    mq_pop = function(n){
      out <- private$mq_list(n = n)
      new_head <- private$mq_get_head() + nrow(out)
      private$mq_set_head(new_head)
      out
    },
    mq_push = function(title, message){
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
    initialize = function(path, create = TRUE){
      self$path <- path
      self$db <- file.path(self$path, "db")
      self$head <- file.path(self$path, "head")
      self$lock <- file.path(self$path, "lock")
      if (create){
        fs::file_chmod(fs::dir_create(self$path), mode = "777")
        private$mq_exclusive({
          fs::file_chmod(fs::file_create(self$db), mode = "777")
          fs::file_chmod(fs::file_create(self$head), mode = "777")
          if (length(private$mq_get_head()) < 1){
            private$mq_set_head(1)
          }
        })
      }
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
    pop = function(n = 1){
      private$mq_exclusive(private$mq_pop(n = n))
    },
    push = function(title, message){
      private$mq_exclusive(private$mq_push(title = title, message = message))
    },
    destroy = function(){
      unlink(self$path, recursive = TRUE, force = TRUE)
    }
  )
)
