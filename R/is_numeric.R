# The is_xxx() functions derived from {checkmate} are like test_xxx() functions,
# but they also record the message in .op$message for possible further use.

#' Assert that an argument is a vector of type numeric
#'
#' Vectors of storage type **integer** and **double** count as **numeric**, c.f. [is.numeric()]. To explicitly assert for integer or double vectors, see [is_integer()], [is_integerish()] or [is_double()].
#'
#' @param x An R object to check.
#' @param lower Lower value (number) all elements of `x` must be greater than or
#' equal to.
#' @param upper Upper value (number) all elements of `x` must be lower than or
#' equal to.
#' @param finite Logical, indicating whether all elements of `x` must be finite,
#' default is `FALSE`.
#' @param any.missing Logical, indicating whether `x` may contain missing values,
#' default is `TRUE`.
#' @param all.missing Logical, indicating whether `x` may be entirely missing
#' values, default is `TRUE`. An empty vector has no missing values.
#' @param len Expected length of `x` (integer).
#' @param min.len Minimal length of `x` (integer).
#' @param max.len Maximal length of `x` (integer).
#' @param unique Logical, indicating whether all values of `x` must be unique,
#' default is `FALSE`.
#' @param sorted Logical, indicating whether all values of `x` must be sorted in
#' ascending order, default is `FALSE`.
#' @param names Check for names. Default in `NULL` (no check). Could be
#' `"unnamed"` (has no names), `"named"` (has names), `"unique"` (has unique
#' names), `"strict"` (same as unique, but names must be also valid R variable
#' names), or `"ids"` (same as strict but not enforce uniqueness).
#' @param typed.missing If `FALSE` (default), all types of missing values (`NA`, `NA_integer_`, `NA_real_`, or `NA_character_`) and empty vectors are allowed while type-checking atomic input. If `TRUE`, leads to strict type checking.
#' @param null.ok If set to `TRUE`, x may also be `NULL`. In this case only a
#' type check of `x` is performed, all additional checks are disabled. Default
#' is `FALSE`.
#'
#' @returns Logical for `is_numeric()`, `TRUE` if `x` passes all checks, `FALSE`
#' otherwise, and the internal `message` option is set with the indication of
#' what failed, can be reused by `error_numeric()` that always create an error
#' message.
#' @author Derived from code by Michel Lang, Bernd Bischl, and Dénes Tóth
#' (authors of the \{checkmate\} package whose code is repackaged here).
#' Documentation is also largely inspired from \{checkmate\} corresponding
#' documentation.
#' @seealso [checkmate::check_numeric()]
#' @export
#' @importFrom checkmate check_numeric
#'
#' @examples
#' is_numeric(1.2) # Better using is_num() in this simple case
#' is_numeric("a")
#' svAssert:::.checkmate_message() # Get the message set by is_numeric()
#'
#' my_log <- function(x) {# x must be numeric >= 0
#'   is_numeric(x, min.len = 1, lower = 0) || stop_numeric(x)
#'   log(x)
#' }
#' my_log(1)
#' try(my_log(-1))
#'
#' # One can also do:
#' #is_ <- is.logical # For more compact syntax
#' #is_(check_numeric(x, min.len = 1, lower = 0) -> m) || error_numeric(x, m)
is_numeric <- .check_to_is_function(checkmate::check_numeric)

# This is what I can do... but it results in a function that takes 2-3x more
# time to run on small objects (sic!)
is_numeric2 <- function(x, lower = -Inf, upper = Inf,
    finite = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL,
    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE,
    names = NULL, typed.missing = FALSE, null.ok = FALSE) {

  is.logical(.op$message <- check_numeric(x, lower, upper, finite,
    any.missing, all.missing, len, min.len, max.len, unique, sorted, names,
    typed.missing, null.ok))
}

#' @rdname is_numeric
#' @param x The R object that was tested, typically with `is_numeric()`.
#' @param msg An optional custom error message. If `NULL` (default), a
#' standard message is created indicating that `x` is not numeric.
#' @param arg The argument name, as a **string**, default is the expression
#' provided to `x`.
#' @param id An optional identifier to append to the error class (like
#' `fun_arg_id` in case you need to differentiate two similar errors.
#' @param call The call where the error was generated. The default computes the
#' top-level call of the function(s) that called `error_numeric()` using
#' [stop_top_call()].
#' @export
stop_numeric <- function(x, msg = NULL, arg = substitute(x), id = NULL,
    call = NULL) {
  chk_msg <- .checkmate_message() %||% ""
  arg <- .op$arg %||% arg
  msg <- .op$message %||% msg %||% c(
    gettext("{.arg {arg}} is not suitable."), i = "{chk_msg}")
  .op$arg <- NULL
  .op$message <- NULL
  stop(msg, class = error_class(id = id), call = call %||% stop_top_call(2L))
}
