#' Assertions and Meaningful Error Messages for 'SciViews::R'
#'
#' The \{svAssert\} package provides fast and versatile assertion functions
#' using a couple `is_xxx()` and `stop_is_xxx()` to check function arguments or
#' other conditions (defensive programming). It also provides an enhanced
#' `stop()` to generate meaningful error messages.
#'
#' @section Important functions:
#'
#' - [stop_()], [warning_()] and other functions described on the same help
#'   page: enhanced stops and warnings.
#' - [is_numeric()]/[stop_is_numeric()]: assertion on numeric vectors.

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate qtest
#' @importFrom cli cli_abort cli_text
#' @importFrom rlang abort
## usethis namespace: end
NULL
