mc_lock <- function(code, db){
  on.exit(filelock::unlock(db_lock))
  db_lock <- filelock::lock(fs::path_ext_set(db, "lock"))
  if (!file.exists(db)){
    return()
  }
  force(code)
}

mc_ack <- function(msg){
  mc_lock(liteq::ack(msg), msg$db)
}

mc_delete_queue <- function(queue){
  mc_lock(liteq::delete_queue(queue), queue$db)
}

mc_ensure_queue <- function(name, db){
  mc_lock(liteq::ensure_queue(name = name, db = db), db)
}

mc_list_messages <- function(queue){
  mc_lock(liteq::list_messages(queue), queue$db)
}

mc_publish <- function(queue, title, message){
  mc_lock(
    liteq::publish(queue = queue, title = title, message = message),
    queue$db
  )
}

mc_requeue_failed_messages <- function(queue){
  mc_lock(liteq::requeue_failed_messages(queue), queue$db)
}

mc_try_consume <- function(queue){
  mc_lock(liteq::try_consume(queue), queue$db)
}

mc_consume <- function(queue){
  mc_lock(liteq::consume(queue), queue$db)
}

mc_delete_queue <- function(queue, force = TRUE){
  mc_lock(liteq::delete_queue(queue, force = force))
}
