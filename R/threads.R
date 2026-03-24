#' Get or set the number of OpenMP threads used by svAssert
#'
#' @description
#' `get_threads()` returns the current number of threads used for parallel
#'   operations. `set_threads()` sets it.
#'
#' @param n A positive integer giving the number of threads. Use `1` to
#'   disable parallelism.
#'
#' @return `get_threads()` returns an integer. `set_threads()` returns the new
#'   value invisibly.
#'
#' @export
#' @useDynLib svAssert c_get_threads
#' @examples
#' (nthreads <- svAssert::get_threads()) # 1 by default
#' (svAssert::set_threads(parallel::detectCores() - 1L))
#' # Now svAssert function use parallelism...
#' # ... your code here...
#' # Reset it
#' (svAssert::set_threads(nthreads))
#' rm(nthreads)
get_threads <- function() {
  .Call(c_get_threads)
}

#' @rdname get_threads
#' @export
#' @useDynLib svAssert c_set_threads
set_threads <- function(n) {
  n <- as.integer(n)
  # TODO: do not allow more than max available threads
  if (length(n) != 1L || is.na(n) || n < 1L)
    stop("'n' must be a positive integer")
  invisible(.Call(c_set_threads, n))
}
