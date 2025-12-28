#' Assert that an argument is a vector of type numeric
#'
#' Vectors of storage type **integer** and **double** count as **numeric**, c.f.
#' [is.numeric()]. To explicitly assert for integer or double vectors, see
#' [is_integer()], [is_integerish()] or [is_double()].
#'
#' @param x An R object to check.
#' @param lower Lower value (number) all elements of `x` must be greater than or
#' equal to.
#' @param upper Upper value (number) all elements of `x` must be lower than or
#' equal to.
#' @param finite Logical, indicating whether all elements of `x` must be finite,
#' default is `FALSE`.
#' @param any.missing Logical, indicating whether `x` may contain missing
#' values, default is `TRUE`.
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
#' @param typed.missing If `FALSE` (default), all types of missing values (`NA`,
#' `NA_integer_`, `NA_real_`, or `NA_character_`) and empty vectors are allowed
#' while type-checking atomic input. If `TRUE`, leads to strict type checking.
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
#'   is_numeric(x, min.len = 1, lower = 0) || stop_is_numeric(x)
#'   log(x)
#' }
#' my_log(1)
#' try(my_log(-1))
is_numeric <- .check_to_is_function(checkmate::check_numeric)

#' @rdname is_numeric
#' @param x The R object that was tested, typically with `is_numeric()`.
#' @param ... Any additional arguments (not checked, and not used).
#' @param msg An optional custom error message. If `NULL` (default), a
#' standard message is created indicating that `x` is not numeric.
#' @param arg The argument name, as a **string**, default is the expression
#' provided to `x`.
#' @param mod An optional modifier string, or `NULL` for none (default). Only
#' `"!"` is considered here, indicating negation of the condition.
#' @param id An optional identifier to append to the error class.
#' @param call The call where the error was generated. The default computes the
#' top-level call of the function(s) that called `error_numeric()` using
#' [stop_top_call()].
#' @export
stop_is_numeric <- function(x, ..., msg = NULL, arg = substitute(x), mod = NULL,
    id = NULL, call = NULL) {

  arg <- .op$arg %||% arg
  .op$arg <- NULL # Just to be sure...

  chk_msg <- .checkmate_message() %||% ""

  # mod "any" or "all" have no effect on functions returning a single logical
  is_not <- startsWith(mod %||% "", "!")
  # is_not == TRUE is a problem because is_numeric() is not really negatable!
  info <- if (is_not) gettext("The contrary of \"{chk_msg}\"") else "{chk_msg}"

  msg <- .op$message %||% msg %||% c(
    gettext("Can't use argument {.arg {arg}} ({.code {x}})."), i = info)

  stop_(msg, class = error_class(id = id), call = call %||% stop_top_call(2L))
}
