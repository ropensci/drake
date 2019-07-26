# Some installations of R require the && and || operators
# to return a result of length 1.
# For example, `nzchar(letters) && length(letters)` fails on
# some platforms but not others. Below, we mock the operators
# to preempt these elusive failures.
# Below, toggle the if() condition on in test mode
# and off in production mode.
# nocov start
if (FALSE) {
  `&&` <- function(x, y) {
    if (length(x) != 1) {
      stop("length x not 1")
    } else if (!x) {
      return(x)
    }
    if (length(y) != 1) {
      stop("length y not 1")
    }
    y
  }

  `||` <- function(x, y) {
    if (length(x) != 1) {
      stop("length x not 1")
    } else if (x) {
      return(x)
    }
    if (length(y) != 1) {
      stop("length y not 1")
    }
    y
  }
}
# nocov end
