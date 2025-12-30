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
stop_equals <- function(lhs, rhs, ..., par. = list()) {
  # TODO: implement any, all, !any and !all modifiers

  .__top_call__. <- FALSE

  arg <- lbl(.op$arg %||% par.$arg %||% substitute(lhs))
  arg2 <- lbl(.op$arg2 %||% par.$arg2 %||% substitute(rhs))

  msg2 <- mod_content(lhs, arg)
  msg3 <- mod_content(rhs, arg2)

  if (mod_not(par.$mod)) {
    msg1 <- c("!" = gettextf("{.code %s} must be different from {.code %s}.",
      arg, arg2))
    # Special case: if mod is "!" and one of the two sides is a constant,
    # we don't need further details
    if (is.null(msg2)) {
      msg3 <- NULL
    } else if (is.null(msg3)) {
      msg2 <- NULL
    } else { # Both sides are complex expressions, and are the same
      msg2 <- c("*" = gettextf(
        "Both {.code %s} and {.code %s} contain {.val %s}.",
        arg, arg2, lbl(lhs)))
      msg3 <- NULL
    }
  } else {
    msg1 <- c("!" = gettextf("{.code %s} must be equal to {.code %s}.",
      arg, arg2))
  }

  stop(par.$msg, msg1, msg2, msg3, par.$footer, class_id = par.$id,
    call = par.$call)
}

#' @rdname stop_equals
#' @export
`stop_==` <- stop_equals

mod_content <- function(x, expr) {
  if (x == expr)
    return(NULL)

  if (is.call(expr)) {
    msg <- switch(as.character(expr[[1]]),
      length = gettextf("length of {.code %s}", lbl(expr[[2]])),
      nrow =,
      NROW = gettextf("number of rows of {.code %s}", lbl(expr[[2]])),
      ncol =,
      NCOL = gettext("number of columns of {.code %s}", lbl(expr[[2]])),
      sprintf("{.code {%s}}", lbl(expr))
    )
  } else {
    msg <- sprintf("{.code {%s}}", lbl(expr))
  }
  c("*" = gettextf("%s is {.val %s}.", msg, x))
}
