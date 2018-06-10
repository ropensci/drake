message_queue <- function(path){
  R6_message_queue$new(path = path)
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
    mq_log = function(){
      if (length(scan(self$db, quiet = TRUE, what = character())) < 1){
        return(
          data.frame(
            title = character(0),
            message = character(0),
            stringsAsFactors = FALSE
          )
        )
      }
      private$parse_db(
        read.table(
          self$db,
          sep = "|",
          stringsAsFactors = FALSE,
          header = FALSE,
          quote = ""
        )
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
      private$parse_db(
        read.table(
          self$db,
          sep = "|",
          skip = private$mq_get_head() - 1,
          nrows = n,
          stringsAsFactors = FALSE,
          header = FALSE,
          quote = ""
        )
      )
    },
    parse_db = function(x){
      colnames(x) <- c("title", "message")
      x$title <- base64url::base64_urldecode(x$title)
      x$message <- base64url::base64_urldecode(x$message)
      x
    }
  ),
  public = list(
    path = character(0),
    db = character(0),
    head = character(0),
    lock = character(0),
    initialize = function(path){
      self$path <- fs::dir_create(path)
      self$db <- file.path(self$path, "db")
      self$head <- file.path(self$path, "head")
      self$lock <- file.path(self$path, "lock")
      private$mq_exclusive({
        fs::file_create(self$db)
        fs::file_create(self$head)
        if (length(private$mq_get_head()) < 1){
          private$mq_set_head(1)
        }
      })
    },
    count = function(){
      private$mq_exclusive(private$mq_count())
    },
    empty = function(){
      self$count() < 1
    },
    log = function(){
      private$mq_exclusive(private$mq_log())
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
