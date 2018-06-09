mc_lock <- function(code, db){
  on.exit(filelock::unlock(db_lock))
  db_lock <- filelock::lock(fs::path_ext_set(db, "lock"))
  force(code)
}

mc_ack <- function(msg){
  mc_lock({
      try(DBI::dbDisconnect(msg$lock), silent = TRUE)
      liteq::ack(msg)
    },
    msg$db
  )
}

mc_ensure_queue <- function(db){
  mc_lock(liteq::ensure_queue(name = "jobs", db = db), db)
}

mc_list_messages <- function(queue){
  mc_lock(liteq::list_messages(queue), queue$db)
}

mc_count_messages <- function(queue){
  nrow(mc_list_messages(queue))
}

mc_publish <- function(queue, title, message){
  mc_lock(
    liteq::publish(queue = queue, title = title, message = message),
    queue$db
  )
}

mc_try_consume <- function(queue){
  mc_lock(liteq::try_consume(queue), queue$db)
}
