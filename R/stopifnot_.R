#' Assert that expressions are all TRUE, with extended error messages
#'
#' If any of the expressions (in ...) are not [all] TRUE, an error is raised. If
#' a `stop_expr()` function exists for the expression `expr`, it is called to
#' generate the error message, otherwise a default message is created.
#'
#' @details
#' `get_mod()` and `mod_not()` are utility functions to extract and manage
#' modifiers ([!], [any()] or [all()]) in expressions to build error messages.
#'
#' @param ... Any number of R expressions, which should each evaluate to (a
#' logical vector of all) [TRUE].
#'
#' @returns For `stopifnot_()`, [NULL] if all statements in ... are TRUE.
#' @export
#' @seealso [stop_()], [base::stop()]
#'
#' @examples
#' stopifnot_(1 == 1, all.equal(pi, 3.14159265), 1 < 2) |> try()
#' stopifnot_(is.character(letters), length(letters) == 1) |> try()
#' stopifnot_(all.equal(pi, 3.141593), 2 < 2, (1:10 < 12), "a" < "b") |> try()
stopifnot_ <- function(...) {
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

        # Run stop_xxx() function, if found
        get_stop(expr = cl.i, call_it = TRUE, force_stop = FALSE)

        # Here is the regular stopifnot() treatment in case not stopped yet
        msg <- if (is.call(cl.i) && identical(1L, pmatch(quote(all.equal),
          cl.i[[1]])) && (is.null(ni <- names(cl.i)) ||
              length(cl.i) == 3L || length(cl.i <- cl.i[!nzchar(ni)]) == 3L))
          sprintf(gettext("%s and %s are not equal:\n  %s"),
            Dparse(cl.i[[2]]), Dparse(cl.i[[3]]), abbrev(r))
        else sprintf(ngettext(length(r), "%s is not TRUE",
          "%s are not all TRUE"), Dparse(cl.i))
      }
      # TODO: replace this by a better stop
      stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
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
#' @returns For `get_stop(call_it = FALSE)`, a list with `stop_fun` the name of
#' the stop function (that exists) or `NULL`otherwise, `call` the complete call
#' to the stop function, `mod` the modifier, and `expr` the core expression.
#' `mod` can be `NULL` or `""` if no modifier is present. Otherwise, it is "!"
#' (not true), "any" (at least one element is true), "all" (all elements are
#' true), "!any" (all elements are wrong), or "!all" (at least one element is
#' wrong). The error message build by your `stop_xxx()` functions should take
#' this modifier into account to issue the correct error message for the core
#' expression.
#' @examples
#'
#' # get_stop() returns infos about a stop function when call_it = FALSE
#' get_stop(length(letters) == 1L, call_it = FALSE)
#' get_stop(length("a") != 1L, call_it = FALSE) # mod == "!", expr = "=="
#' get_stop(!any(c(TRUE, TRUE, NA)), call_it = FALSE) # mod == "!any"
#' # all! is the same as !any (and any! is !all)
#' get_stop(all(!c(TRUE, FALSE, NA)), call_it = FALSE) # mod == "!any"
#' # Contrived and weird example: it got simplified for mod
#' get_stop(all(any(!!all(!anyNA(x)))), call_it = FALSE) # mod == "!any"
#' # Call the stop function (if it exists) to raise the error
#' get_stop(is.numeric(letters) && length(letters) > 0L) |> try()
get_stop <- function(x, expr = substitute(x), par. = list(), call_it = TRUE,
    force_stop = TRUE) {

  if (!is.list(par.)) {
    warning("`par.` should be a list, ignoring it.")
    par. <- list()
  }

  corex <- expr
  funs <- c("any", "all", "!", "(", "!=", "<=", ">=")
  mod <- par.$mod %||% ""
  while (is.call(corex) && (mod1 <- as.character(corex[[1]])) %in% funs) {
    mod <- switch(mod1,
      "!=" =,
      "<=" =,
      ">=" = paste0(mod, "!"),
      paste0(mod, mod1)
    )
    corex <- switch(mod1,
      "!=" = call("==", corex[[2]], corex[[3]]),
      "<=" = call(">", corex[[2]], corex[[3]]),
      ">=" = call("<", corex[[2]], corex[[3]]),
      corex[[2]]
    )
  }

  # Simplify mod:
  # - Eliminate parentheses
  mod <- gsub("(", "", mod, fixed = TRUE)
  # - Eliminate any and all before the last one, if they appear multiple times
  #   (any() or all() applies multiple times have no effect past first one)
  #   a) Protect last one using sentence case (Any or All)
  mod <- sub("(a)([nl][yl]!*)$", "A\\2", mod)
  #   b) Eliminate all remaining lowercase any or all
  mod <- gsub("any", "", mod, fixed = TRUE)
  mod <- gsub("all", "", mod, fixed = TRUE)
  #   c) Restore last one to lowercase
  mod <- sub("A", "a", mod, fixed = TRUE)
  # - Eliminate double negations a first time
  mod <- gsub("!!", "", mod, fixed = TRUE)
  # - all! is !any and any! is !all for last one
  mod <- sub("all!", "!any", mod)
  mod <- sub("any!", "!all", mod)
  # - Eliminate double negations a second time (in case of !all! or !any!)
  mod <- gsub("!!", "", mod, fixed = TRUE)
  # Inject computer mod in par.
  par.$mod <- mod

  # Compute the name of the default stop_xxx() function and the call
  call <- NULL
  if (is.call(corex)) {
    stop_fun <- paste0("stop_", corex[[1]])
    if (exists(stop_fun, mode = "function")) {
      call <- corex
      call[[1]] <- as.symbol(stop_fun)
      call$par. <- par.
    }
  } else if (is.name(corex)) {
    stop_fun <- fun_name <- paste0("stop_", corex, "_") # Note the trailing '_'
    if (!exists(fun_name, mode = "function"))
     cal <- call(stop_fun, corex, par. = par.)
  } else {
    stop_fun <- NULL
  }
  if (isTRUE(call_it)) {# Call the stop function
    if (!is.null(call)) {
      rlang::eval_bare(call, parent.frame()) # This is supposed to stop
    }
    # If stop function not found, or it does not stop
    if (isTRUE(force_stop))
      stop("{.code {arg_or_code(expr = expr)}} is not TRUE")
  }
  list(stop_fun = stop_fun, call = call, mod = mod, expr = corex)
}

#' @rdname stopifnot_
#' @param width Maximum width of the deparsed expression
#' @param nlines Maximum number of lines for the deparsed expression
#' @export
#' @returns `arg_or_code()` returns "\{.arg \{x\}\}" if x is a symbol, or
#' \{.code \{x\}\} otherwise.
#' otherwise.
#' @examples
#'
#' # arg_or_code() helps building error messages for expressions
#' y <- as.symbol('data')
#' arg_or_code(y)
#' y <- quote(toupper(1:10))
#' arg_or_code(y)
arg_or_code <- function(expr, width = 30L, nlines = 1L) {
  res <- deparse1(expr, width.cutoff = width + 3L, nlines = nlines)
  if (nchar(res) > width)
    res <- paste(substr(res, 1L, width - 4L), "...")
  res
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
#'   expr <- substitute(x)
#'   arg <- .op$arg %||% par.$arg %||% arg_or_code(expr)
#'
#'   # length(x) == 1 always returns a single logical, can use mod_not() here
#'   if (mod_not(par.$mod)) {
#'     stop(par.$msg, "!" = "{.code {arg}} cannot have length 1.", par.$footer,
#'       class = error_class(id = par.$id),
#'       call = par.$call %||% stop_top_call(2L))
#'   } else {
#'     stop(
#'       par.$msg, "!" = "{.code {arg}} must have length 1",
#'       "*" = "Its length is {length(x)}.",
#'       class = error_class(id = par.$id),
#'       call = par.$call %||% stop_top_call(2L))
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
mod_not <- function(mod) {
  !is.null(mod) && mod != "" && startsWith(mod, "!")
}

