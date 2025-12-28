#' Stop function for equality or inequality check
#'
#' `stop_equals()` generates an error message for both `==` and `!=` assertions.
#'
#' @param lhs The left-hand side of the comparison.
#' @param rhs The right-hand side of the comparison.
#' @param ... Additional arguments (not use)d yet).
#' @param par. A list of parameters to customize the error message. Optional
#' fields include `msg` (message(s) to display at the top), `footer`(message(s)
#' to add at the bottom), `arg` (name of first argument), `arg2` (name of second
#' argument), `id` (error class id), `mod` (the modifier, that is "" for `==`
#' and "!" for `!=`) `call` (the call where the error was generated).
#'
#' @returns This function always stops with an error.
#' @export
#'
#' @examples
#' stop_equals(1, 2) |> try()
#' x <- 1
#' stop_equals(x, 2) |> try()
#' stop_equals(x, 1, par. = list(mod = "!")) |> try()
#' y <- 2
#' stop_equals(x, y) |> try()
#' z <- 1
#' stop_equals(x, z, par. = list(mod = "!")) |> try()
#' stop_equals(length(x), 2L) |> try()
#' # Extra message
#' stop_equals(2, x, par. = list(msg = c("Some info...", x = "This is wrong"),
#'   footer = c("*" = "A footer..."))) |> try()
stop_equals <- `stop_==` <- function(lhs, rhs, ..., par. = list()) {
  # TODO: implement any, all, !any and !all modifiers
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  arg <- .op$arg %||% par.$arg %||% arg_or_code(lhs)
  arg2 <- .op$arg2 %||% par.$arg2 %||% arg_or_code(rhs)

  msg2 <- msg_content(lhs)
  msg3 <- msg_content(rhs)

  if (mod_not(par.$mod)) {
    msg1 <- gettext("!" = "{.code {arg}} must be different from {.code {arg2}}.")
    # Special case: if mod is "!" and one of the two sides is a constant,
    # we don't need further precisions
    if (is.null(msg2)) {
      msg3 <- NULL
    } else if (is.null(msg3)) {
      msg2 <- NULL
    } else { # Both sides are complex expressions, and are the same
      x <- as.character(lhs)
      y <- as.character(rhs)
      msg2 <- c("*" = gettextf(
        "Both {.code %s} and {.code %s} are {.val {%s}}.", x, y, y))
      msg3 <- NULL
    }
  } else {
    msg1 <- gettext("!" = "{.code {arg}} must be equal to {.code {arg2}}.")
  }

  stop(par.$msg, msg1, msg2, msg3, par.$footer,
    class = error_class(id = par.$id), call = par.$call %||% stop_top_call(2L))
}

msg_content <- function(expr) {
  if (is.symbol(expr) || is.call(expr)) {
    label <- arg_or_code(expr)
    c("*" = gettextf("{.code %s} is {.val {%s}}.", label, deparse(expr)))
  } else NULL
}

