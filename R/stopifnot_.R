# TODO: better error class than just stopifnot__error

#' Assert that expressions are all TRUE, with extended error messages
#'
#' If any of the expressions (in ...) are not [all] TRUE, an error is raised. If
#' a `stop_expr()` function exists for the expression `expr`, it is called to
#' generate the error message, otherwise a default message is created.
#'
#' @details
#' `get_stop_fun()` and `mod_not()` are utility functions to compute the stop
#' function call and manage a modifier ([!], [any()] or [all()]) in expressions
#' to ease building error messages.
#'
#' @param ... Any number of R expressions, which should each evaluate to (a
#' logical vector of all) [TRUE].
#'
#' @returns For `stopifnot_()`, [NULL] if all statements in ... are TRUE.
#' @export
#' @seealso [stop_()], [base::stop()]
#'
#' @examples
#' # stop <- stop_
#' # Note that |> try() is just there to catch error; do not use in your code!
#' stopifnot_(1 == 1, all.equal(pi, 3.14159265), 1 < 2) |> try()
#' stopifnot_(is.character(letters), length(letters) == 1) |> try()
#' stopifnot_(all.equal(pi, 3.141593), 2 < 2, (1:10 < 12), "a" < "b") |> try()
stopifnot_ <- function(...) {
  # Code adapted from base::stopifnot() v.4.4.3
  Dparse <- function(call, cutoff = 60L) {
    ch <- deparse(call, width.cutoff = cutoff)
    if (length(ch) > 1L)
      paste(ch[1L], "....")
    else ch
  }
  head <- function(x, n = 6L) x[seq_len(if (n < 0L) max(length(x) +
      n, 0L) else min(n, length(x)))]
  abbrev <- function(ae, n = 3L) paste(c(head(ae, n), if (length(ae) >
      n) "...."), collapse = "\n  ")

  n <- ...length()
  for (i in seq_len(n)) {
    r <- ...elt(i)
    if (!(is.logical(r) && !anyNA(r) && all(r))) {
      dots <- match.call()[-1L]
      if (is.null(msg <- names(dots)) || !nzchar(msg <- msg[i])) {
        cl.i <- dots[[i]]

        # More translations (Mean absolute|relative|scaled difference:)
        if (is.character(r))
          r <- translate(r, get('general_msgs'))

        # Run stop_xxx() function, if found
        get_stop_fun(expr = cl.i, call_it = TRUE, force_stop = FALSE)

        # Here is the regular stopifnot() treatment in case not stopped yet
        msg <- if (is.call(cl.i) && identical(1L, pmatch(quote(all.equal),
          cl.i[[1]])) && (is.null(ni <- names(cl.i)) ||
              length(cl.i) == 3L || length(cl.i <- cl.i[!nzchar(ni)]) == 3L))
          #sprintf(gettext("%s and %s are not equal:\n  %s"),
          #  Dparse(cl.i[[2]]), Dparse(cl.i[[3]]), abbrev(r))
          c(sprintf(gettext("{.code %s} and {.code %s} are not equal"),
            Dparse(cl.i[[2]]), Dparse(cl.i[[3]])),
            i = abbrev(r))
        else sprintf(ngettext(length(r), "{.code %s} is not {.val TRUE}",
          "{.code %s} are not all {.val TRUE}"), Dparse(cl.i))
      }
      #stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
      stop(msg, class_id = "stopifnot")
    }
  }
  invisible()
}

#' @rdname stopifnot_
#' @param x An expression to analyze
#' @param expr An expression object
#' @param mod A modifier string (could be `""`, `"!"`, `"any"`, `"all"`,
#' `"!any"`, `"!all"`, or any combination of these).
#' @param par. A list of additional parameters to pass to the stop function,
#' optionally containing  `msg` a string with custom message(s), `arg` a single
#' string with the name of the argument that failed the assertion, `mod` a
#' single string with starting modifier (it is recomputed), `id` an
#' identifier to append to the error class, or `call` the call where the error
#' was generated. Default is an empty list.
#' @param call_it If `TRUE`, the corresponding `stop_xxx()` function is called
#' (if it exists) to generate the error message. If `FALSE`, see `force_stop`.
#' Default is `TRUE`.
#' @param force_stop If `call_it` is `TRUE` and no `stop_xxx()` function exists
#' for the core expression, or if it exists but does not stop, and if
#' `force_stop = TRUE` (default), a generic error message is generated and the
#' function stops. If `force_stop = FALSE`, no error is raised and information
#' about the stop function is returned.
#' @export
#' @returns For `get_stop_fun(call_it = FALSE)`, a list with `stop_fun` the name
#' of the stop function, `call` the call to the stop function (if it exists,
#' otherwise `NULL`), `mod` the modifier, and `expr` the core expression,
#' excluding the modifier.
#' `mod` can be `NULL` or `""` if no modifier is present. Otherwise, it is "!"
#' (not true), "any" (at least one element is true), "all" (all elements are
#' true), "!any" (all elements are wrong), or "!all" (at least one element is
#' wrong). The error message build by your `stop_xxx()` functions should take
#' this modifier into account to issue the correct error message for the core
#' expression.
#' @examples
#'
#' # get_stop_fun() returns infos about a stop function when call_it = FALSE
#' get_stop_fun(length(letters) == 1L, call_it = FALSE)
#' get_stop_fun(length("a") != 1L, call_it = FALSE) # mod == "!", expr = "=="
#' get_stop_fun(!any(c(TRUE, TRUE, NA)), call_it = FALSE) # mod == "!any"
#' # all! is the same as !any (and any! is !all)
#' get_stop_fun(all(!c(TRUE, FALSE, NA)), call_it = FALSE) # mod == "!any"
#' # Contrived and weird example: it got simplified for mod
#' get_stop_fun(all(any(!!all(!anyNA(x)))), call_it = FALSE) # mod == "!any"
#' # Call the stop function (if it exists) to raise the error
#' get_stop_fun(is.numeric(letters) && length(letters) > 0L) |> try()
get_stop_fun <- function(x, expr = substitute(x), par. = list(), call_it = TRUE,
    force_stop = TRUE) {

  # In case wrong par., ignore it with a warning (critical code, no stop here!)
  if (!is.list(par.)) {
    warning("`par.` should be a list, ignoring it.")
    par. <- list()
  }

  # Extract the mod(ifier) and the core_expr(ession)
  ex <- .extract_core_expression(expr, par.$mod %||% "")
  core_expr <- ex$core_expr
  # Simplify mod(ifier), e.g., '!!all(!any' is functionnaly equivalent to '!any'
  par.$mod <- mod <- .simplify_modifier(ex$mod)

  # Compute the name of the default stop_xxx() function and the call
  stop_fun_call <- .stop_fun_call(core_expr, par.)

  # Do we execute the call now?
  if (isTRUE(call_it)) {
    if (!is.null(stop_fun_call$call))
      rlang::eval_bare(stop_fun_call$call, parent.frame()) # Supposed to stop

    # If stop function not found (call == NULL), or it does not stop...
    # force stop now with a generic error message
    if (isTRUE(force_stop))
      stop("{.code {lbl(expr = expr)}} is not TRUE")
  }

  # Return the computed components
  list(stop_fun = stop_fun_call$stop_fun, call = stop_fun_call$call,
    mod = mod, expr = core_expr)
}

# Separate an expression into mod(ifiers) and a core expression
# ex.: !any(length(x) == 1) -> mod = "!any", core expr = length(x) == 1
# expr: expression to analyze
# mod: initial modifier (usually "")
.extract_core_expression <- function(expr, mod) {
  # Functions that can modify the expression
  funs <- c("any", "all", "!", "(", "!=", "<=", ">=")

  while (is.call(expr) && (one_mod <- as.character(expr[[1]])) %in% funs) {
    # != is transformed into == with mod "!"
    # <= is transformed into > with mod "!"
    # >= is transformed into < with mod "!"
    # This way we need only three cases instead of six
    # and, e.g., !x == 1 or x != 1 are handled the same way
    mod <- switch(one_mod,
      "!=" =,
      "<=" =,
      ">=" = paste0(mod, "!"),
      paste0(mod, one_mod)
    )
    expr <- switch(one_mod,
      "!=" = call("==", expr[[2]], expr[[3]]),
      "<=" = call(">", expr[[2]], expr[[3]]),
      ">=" = call("<", expr[[2]], expr[[3]]),
      expr[[2]]
    )
  }
  list(mod = mod, core_expr = expr)
}

# Simplify a (mod)ifier string by removing redundant parts
.simplify_modifier <- function(mod) {
  # If already correct (often the case), return it right away
  if (mod %in% c("", "any", "all", "!", "!any", "!all"))
    return(mod)

  # Simplify expression of any complexity
  # - Eliminate parentheses (they are useless in the context)
  mod <- gsub("(", "", mod, fixed = TRUE)
  # - Eliminate any and all before the last one, if they appear multiple times
  #   (any() or all() applies multiple times have no effect past first one)
  #   a) Protect last one using sentence case (Any or All)
  #      Capture any or all followed by any number of '!' at the end
  #      and replace by Any or All with uppercase 'A'
  mod <- sub("(a)([nl][yl]!*)$", "A\\2", mod)
  #   b) Eliminate all remaining lowercase any or all
  mod <- gsub("any", "", mod, fixed = TRUE)
  mod <- gsub("all", "", mod, fixed = TRUE)
  #   c) Restore last one Any or All to lowercase any or all
  mod <- sub("A", "a", mod, fixed = TRUE)
  # - Eliminate double negations a first time
  mod <- gsub("!!", "", mod, fixed = TRUE)
  # - all! is the same as !any and any! is the same as !all for last one
  #   In order to limit the number of cases to deal with, we transform these
  #   to always have '!' in a leading position in the mod(ifier)
  mod <- sub("all!", "!any", mod)
  mod <- sub("any!", "!all", mod)
  # - Eliminate double negations a second time (in case of !all! or !any!)
  mod <- gsub("!!", "", mod, fixed = TRUE)
  mod
}

# Compute the name of a stop_fun(ction), and its call (if it exists)
# core_expr: the core expression, as computed from original expr(ession) by
#   .extract_core_expression()
# par.: list of additional parameters to pass to the stop function
.stop_fun_call <- function(core_expr, par.) {
  call <- NULL

  # core_expr can be a call to a function, a nmae or something else
  if (is.call(core_expr)) {
    # Call to function `fun` -> stop function = `stop_fun`
    stop_fun <- paste0("stop_", core_expr[[1]])
    if (exists(stop_fun, mode = "function")) {# Only compute a call if it exists
      call <- core_expr
      call[[1]] <- as.symbol(stop_fun)
      call$par. <- par.
    }

  } else if (is.name(core_expr)) {# Name `name` -> stop function = `stop_name_`
    # Note the trailing '_' to differentiate it from previous case with a call
    stop_fun <- paste0("stop_", core_expr, "_")
    if (!exists(stop_fun, mode = "function")) # Again only compute if it exists
      call <- call(stop_fun, core_expr, par. = par.)

  } else {# For other cases, both stop_fun and call are NULL
    stop_fun <- NULL
  }
  list(stop_fun = stop_fun, call = call)
}

#' @rdname stopifnot_
#' @export
#' @returns For `mod_not()`, `TRUE` if the modifier starts with "!", `FALSE`
#' otherwise.
#' @examples
#'
#' # mod_not() can be used exclusively to build either a positive, or a negative
#' # sentence in your error message when the expression always returns a single
#' # logical value, because in this case "any" or "all" have no effect.
#' stop_length_one <- function(x, ..., par. = list()) {
#'   arg <- lbl(.op$arg %||% par.$arg %||% substitute(x))
#'
#'   # length(x) == 1 always returns a single logical, can use mod_not() here
#'   # and safely ignore 'any' or 'all' modifiers
#'   if (mod_not(par.$mod)) {
#'     stop(
#'       "!" = "{.code {arg}} cannot have length 1.")
#'   } else {
#'     stop(
#'       "!" = "{.code {arg}} must have length 1",
#'       "*" = "Its length is {length(x)}.")
#'   }
#' }
#' length("a") == 1 || stop_length_one("a")
#' (length(letters) == 1 || stop_length_one(letters)) |> try()
#' # This avoids writing two stop_xxx() functions, one for == and one for !=
#' (length("a") != 1 || stop_length_one("a", par. = list(mod = "!"))) |> try()
#' (!length("a") == 1 || stop_length_one("a", par. = list(mod = "!"))) |> try()
#' # any() or all() have no effect on single logical and can then be ignored
#' (any(length("a") != 1) ||
#'   stop_length_one("a", par. = list(mod = "!all"))) |> try()
#' rm(stop)
mod_not <- function(mod) {
  !is.null(mod) && mod != "" && startsWith(mod, "!")
}

#' @rdname stopifnot_
#' @export
#' @returns For `mod_content()`, a message with the content of `expr` and its
#'   value `x`.
mod_content <- function(x, expr) {
  if (length(x) == 1L && x == expr)
    return(NULL)

  if (is.call(expr)) {
    msg <- switch(as.character(expr[[1]]),
      length = gettext("length of {.code {expr[[2]]}}"),
      nrow =,
      NROW = gettext("number of rows of {.code {expr[[2]]}}"),
      ncol =,
      NCOL = gettext("number of columns of {.code {expr[[2]]}}"),
      "{.code {expr}}"
    )
  } else {
    msg <- "{.code {expr}}"
  }
  msg <- paste(msg, gettext("is {.val {x}}."))
  c("*" = format_inline(msg))
}
