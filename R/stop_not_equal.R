#' Stop function for inequality check
#'
#' `stop_not_equal()` generates an error message for `!=` assertions.
#'
#' @param lhs The left-hand side of the comparison.
#' @param rhs The right-hand side of the comparison.
#' @param ... Additional arguments (not used yet).
#' @param mod A character string indicating the modifier to use for the
#'   comparison (`""`, `"any"`, or `"all"`). Default is `NULL`, which is
#'   equivalent to `""`.
#' @param na.rm Logical, indicating whether missing values should be removed
#'   before performing the comparison. Default is `FALSE`.
#' @param test_it Logical, indicating whether the comparison should be actually
#'   tested. Default is `TRUE`. If `FALSE`, the function only constructs the
#'   error message based on the lengths of `lhs` and `rhs`, and the presence of
#'   missing values.
#' @param par. A list of parameters to customize the error message. Optional
#' fields include `msg` (message(s) to display at the top), `footer`(message(s)
#' to add at the bottom), `arg` (name of first argument), `arg2` (name of second
#' argument), `id` (error class id), `mod` (the modifier, see `mod` above),
#' `call` (the call where the error was generated).
#'
#' @returns This function always stops with an error.
#' @export
#'
#' @examples
#' x <- 1
#' stop_not_equal(x, 1) |> try()
#' x <- 1:2
#' stop_not_equal(x, 1:2, mod = "any") |> try()
#' stop_not_equal(x, 1:2, mod = "all") |> try()
stop_not_equal <- function(lhs, rhs, ..., mod = NULL, na.rm = FALSE,
    test_it = TRUE, par. = list()) {

  .__top_call__. <- FALSE

  arg_name <- .op$arg %||% par.$arg %||% substitute(lhs)
  arg <- lbl(arg_name)
  arg2_name <- .op$arg2 %||% par.$arg2 %||% substitute(rhs)
  arg2 <- lbl(arg2_name)
  .op$arg <- NULL # Just to be sure...
  .op$arg2 <- NULL # Just to be sure...

  #msg2 <- mod_content(lhs, arg)
  #msg3 <- mod_content(rhs, arg2)

  fun <- "stop_not_equal"
  rel <- "!="

  mod <- mod %||% par.$mod %||% ""
  if (!mod %in% c("", "any", "all"))
    stop("Invalid {.var mod} parameter in {.fun {fun}}: {.val {mod}}.",
      i = "Allowed values are '', 'any', and 'all'.",
      .internal = TRUE)

  case <- .case_comparison(lhs, rhs, rel = rel, mod = mod, na.rm = na.rm,
    test_it = test_it)
  msg <- .case_comparison_message(case = case, rel = rel, arg = arg,
    arg2 = arg2, arg_name = arg_name, arg2_name = arg2_name,  x = lhs,
    y = rhs, fun = fun, mod = mod)

  if (length(msg) == 1L && msg == "")
    stop(gettextf("Wrong case in {.fun {fun}}: {.val %s}.", case$code),
      .internal = TRUE)

  stop(par.$msg, msg, par.$footer, .internal = attr(msg, ".internal"))
}

#' @rdname stop_not_equal
#' @export
`stop_!=` <- stop_not_equal
