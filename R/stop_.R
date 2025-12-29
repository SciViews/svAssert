#' Enhanced stop and warning
#'
#' @description

#' `stop_()` is an enhanced version of [stop()] to generate meaningful error
#' with error-recoverable glue interpolation and translation.
#' `warning_()` is similar to [warning()] but uses `call. = FALSE` by default.
#' `error_class()`, `stop_top_call()` and `object_info()` are support functions
#' that help building contextual and explicit error messages.
#'
#' @details
#' `stop_()` is a wrapper around [rlang::abort()] that provides more control
#' on the stop message thanks to [cli::cli_abort()] glue interpolation and
#' [gettext()] translation. It can recover from errors in the formatting and
#' translation processes. In this case, it throws the raw error message with a
#' warning. It also adds a class to the error message, with a default one
#' automatically computed by `error_class()`. It uses `stop_top_call()` to
#' provide a simple mechanism to point to the execution environment of the
#' running function that is relevant in the context. Add a variable
#' `.__top_call__. <- TRUE` in the relevant function, or
#' `.__top_call__. <- FALSE` in an helper function, to be sure to point to the
#' function of interest (most of the time, the function called by the user, see
#' examples).
#' A reminder message inviting to access the backtrace of the error is displayed
#' depending on the `svAssert_backtrace_on_error` option. Set it to `"none"`
#' (disable it), `"reminder"` (default in interactive sessions, show the
#' reminder message), `"branch"` (display a simplified backtrace) or `"full"`
#' (default in non-interactive sessions, display the full tree). You rarely need
#' to change the default. It synchronizes with `rlang_backtrace_on_error` option
#' used in [rlang::abort()] when needed.
#' You can use [rlang::global_entrace()] to format classical base R [stop()]
#' messages in a similar way as `stop_()` and [rlang::abort()] do. You can also
#' use [rlang::last_error()] to revisit the last error message, or
#' [rlang::last_trace()] to inspect the backtrace of the message.
#' Finally, the display of the error message is customisable. See
#' [Customising condition messages](
#' https://rlang.r-lib.org/reference/topic-condition-customisation.html).
#'
#' @param ... One or more character strings with the error or warning
#'   messages to be translated. For `stop_()` only, name them `'*' =`, `'i' =`,
#'   `'v' =`, `'x' =` or `'!' =` to format a bullet-list with the message items.
#'   First message item is considered to use the `'!'` bullet by default. The
#'   messages also support glue interpolation and inline markups,
#'   see [Formatting messages with cli](
#'   https://rlang.r-lib.org/reference/topic-condition-formatting.html)
#'   and [cli::format_inline()]. `warning_()` messages are just concatenated
#'   without extra character (space) in between and without glue interpolation.
#' @param domain The translation. domain, see [gettext()]. If `NA` or `""`,
#'   messages are not translated (use this with messages that are already
#'   translated).
#' @param class The subclass of the error condition message. By default, it is
#'   computed by `error_class()`, using the name of the function in `call` and
#'   the name of the first argument concerned by the error message, if available
#'   (plus, optionally, the `class_id`).
#' @param class_id An optional identifier to append to the error subclass.
#' @param call The execution environment of a currently running function where
#'   the error should be reported from ( called the relevant function).
#' @param parent Give a condition object when an error is rethrown from a
#'   condition handler, such as [withCallingHandlers()] or [rlang::try_fetch()]
#'   to chain errors (see
#'   [Including contextual information with error chains](
#'   https://rlang.r-lib.org/reference/topic-error-chaining.html).
#'   Indicate `NA` for an unchained rethrow, in case you want to rethrow with a
#'   custom error message (do not abuse this, and never hide errors).
#' @param .inherit Logical, whether to inherit parent conditions when chaining
#'   errors. Default is `TRUE`.
#' @param .internal Logical, whether the error is internal to the package. If
#'   `TRUE`, a footer bullet is added to indicate it to invite the user to
#'   report the error to the package authors. Default is `FALSE`.
#' @param .file A connection or a string where to print the message. The default
#'   is context-dependent, see the `stdout` vs `stderr` section in
#'   [rlang::abort().
#' @param .envir The environment where to evaluate the glue expressions.
#' @param .frame The environment to use for the backtrace (usually the same as
#'   `.envir`).
#' @param .trace_bottom An optional environment to truncate the backtrace in
#'   order to display only the most relevant part of it. Default is `NULL` and
#'   it uses `call`if it is an environment, or `.frame` otherwise.
#' @param .last_call The last call issued by the user. Different from `call`
#'   when a first argument with dot `(.)` (e.g., `data = (.)`) was automatically
#'   injected in the call (so-called "data-dot mechanism). In this case, extra
#'   information is added to the error message, except if `.last_call` is `NULL`
#'   (use it to suppress these extra messages).
#'
#' @returns `stop_()` and `warning_()` are invoked for their side-effects, but
#'   `stop_()` actually stops execution of the current code.
#' @export
#' @seealso [rlang::abort()], [cli::cli_abort()], [gettext()] [stop()], [warning()]
#'
#' @examples
#' # If you want to include the error messages in the translation strings in
#' # your package, you have to rename `stop_()` into `stop()` and `warning_()`
#' # into `warning()`, because [tools::xgettext2pot()] will only pick up the
#' # later ones.
#' stop <- stop_
#' warning <- warning_
#'
#' # Call not integrated by default now
#' warning("just a test")
#' warning("just a test", call. = TRUE)
#'
#' if (FALSE) {# Avoid running code that generates errors automatically
#' # Correctly formatted stop messages
#' n <- "some text"
#' stop("{.var n} must be a numeric vector",
#'   x = "You've supplied a {.cls {class(n)}} vector.")
#'
#' n <- 1:18
#' stop("{.var n} must be a scalar numeric:",
#'   i = "There {?is/are} {length(n)} element{?s}.",
#'   x = "Indicate a single numeric, not: {n}.")
#'
#' # When issued from within a function, the function call is used in the error
#' test1 <- function(x) {
#'   stop("{.var n} must be a scalar numeric:",
#'     i = "There {?is/are} {length(x)} element{?s}.")
#' }
#' test1(1:3)
#'
#' # If another function calls `test1()`, error is still reported from test1:
#'
#' test2 <- function(x) {
#'   test1(x)
#' }
#' test2(1:3)
#'
#' # In such a case, it is better to report the error from `test2()`.
#' # You can do that by stating `._top_call_. <- TRUE` in the body of `test2()`.
#' test2 <- function(x) {
#'   .__top_call__. <- TRUE
#'   test1(x)
#' }
#' test2(1:3)
#' }# End of if(FALSE)
#'
#' rm(stop, warning)
stop_ <- function(..., domain = NULL, class = error_class(call,
    class_id = class_id), class_id = .op$class_id, call = NULL,
    parent = NULL, .inherit = TRUE, .internal = FALSE, .file = NULL,
    .envir = parent.frame(), .frame = .envir, .trace_bottom = NULL,
    .last_call = sys.call(-1L)) {

  # Default values for call and class
  if (is.null(call))
    call <- stop_top_call(2L)
  if (is.null(class)) {
    if (!length(class_id)) {
      class_id <- NULL
    } else {
      class_id <- as.character(class_id)[[1]]
    }
    class <- error_class(call, class_id = class_id)
  }

  to_translate <- unlist(list(...)) # ... as a flat list, preserving names
  # Make sure to continue, even with a wrong domain
  if (!length(domain) || anyNA(domain) ||
      !is.character(domain) || domain[1] == "") {
    message <- to_translate # No translation
  } else {
    # Note: if domain is unknown, it returns the message untranslated
    args <- c(as.list(to_translate), list(domain = domain[1], trim = TRUE))
    message <- do.call(gettext, args)
    # Sometimes, gettext() drops names
    names(message) <- names(to_translate)
  }

  # If the data-dot mechanism was activated, give more info in the error message
  if (!is.null(.last_call)) {
    message_data_dot <- .get_data_dot_error_msg(.last_call)
   if (length(message_data_dot))
      message <- c(message, message_data_dot)
  }

  # Equivalent to cli::cli_abort, but catching errors in message formatting
  message_formatted <- try(vapply(message, FUN = format_inline,
    FUN.VALUE = character(1), USE.NAMES = TRUE, .envir = .envir), silent = TRUE)
  if (inherits(message_formatted, "try-error")) {
    warning("Formatting of the error message failed, using unformatted messages.",
      "\n", message_formatted, call. = FALSE)
    class <- c("cli_format_error", class)
    message_formatted <- message
  }

  # Message for the internal error (same one as in abort(), but translatable)
  if (isTRUE(.internal)) {
    message_formatted <- c(
      message_formatted,
      i = gettext(
        "This is an internal error, please report it to the package authors.")
    )
  }

  # Use my custom backtrace message reminder (for translation)
  if ("svAssert_error" %in% class) {
    bt_op <- getOption("svAssert_backtrace_on_error", NULL)
    if ((is.null(bt_op) && interactive()) || bt_op == "reminder") {
      message_formatted <- c(
        message_formatted,
        format_inline(gettext(
          "Run `{.run rlang::last_trace()}` to see where the error occurred."))
      )
      # Avoid displaying it twice
      options(rlang_backtrace_on_error = "none")
    }
  }

  abort(message_formatted, class = class, call = call, parent = parent,
    use_cli_format = TRUE, .inherit = .inherit, .file = .file, .frame = .frame,
    .trace_bottom = .trace_bottom)
}

# If first argument is '(.)', provide extra information about data-dot
.get_data_dot_error_msg <- function(last_call) {
  if (missing(last_call))
    last_call <- sys.call(-2L)
  if (length(last_call) < 2L) {
    first_arg <- ""
  } else {
    first_arg <- deparse1(last_call[[2]])
    if (is.null(first_arg) || is.na(first_arg))
      first_arg <- ""
  }

  # Enhance the message (data-dot mechanisms is likely activated)
  message <- character(0)
  data_dot <- (first_arg == '(.)')
  if (data_dot)
    message <- c(i = gettext(paste(
      "{.emph The data-dot mechanism was likely activated}",
      "(see {.help svBase::data_dot_mechanism}).")))

  if (data_dot || first_arg == '.')
    message <- c(message, `*` = gettext(
      "{.emph {.code .} is {object_info(.)}}"))

  message
}

#' @rdname stop_
#' @export
#' @param call. Logical, whether to include the call in the warning message.
#' @param immediate. Logical, whether to issue the warning immediately even if
#' `getOption("warn") <= 0`. Note that this is not respected for condition
#' objects.
#' @param noBreaks. logical, indicating as far as possible that the message
#'   should be output as a single line when `options(warn = 1)`.
warning_ <- function(..., call. = FALSE, immediate. = FALSE, noBreaks. = FALSE,
    domain = NULL) {
  base::warning(..., call. = call., immediate. = immediate.,
    noBreaks. = noBreaks., domain = domain)
}

#' @rdname stop_
#' @export
#' @param nframe The number of frames to go up the call stack to start finding
#'   the top call (as soon as `.__to_call__.` is found in the environment,
#'   look in its parent frame).
#' @returns
#' `stop_top_call()` returns the top call to be used for stop condition messages
#' (to be used as `call` argument of `stop_()`).
#' call for stop condition messages.
stop_top_call <- function(nframe = 2L) {
  env <- call <- parent.frame(nframe)
  max_frames <- sys.nframe()
  if (max_frames > nframe) {
    for (i in (nframe + 1):max_frames) {
      call <- env
      env <- parent.frame(i)
      if (!isTRUE(env$`.__top_call__.`))
        break
    }
  }
  call
}

#' @rdname stop_
#' @export
#' @returns
#' `error_class()` returns a character string with the error class name computed
#' for `stop_()`, that is, `c("fun_arg1_id", "svAssert_error")`.
error_class <- function(call = parent.frame(), class_id = NULL) {
  mcall <- eval(quote(match.call()), envir = call)
  class_name <- as.character(mcall)
  class_name <- class_name[1:min(2, length(class_name))]
  c(paste(c(class_name, class_id), collapse = '_'), "svAssert_error")
}

#' @rdname stop_
#' @param x An R object to describe.
#' @export
#' @returns
#' `object_info()` returns a character string describing the R object provided.
object_info <- function(x) {
  if (is.null(x)) {
    "'NULL'"
  } else if (is.data.frame(x)) {
    x_names <- paste0("'", names(x), "'")
    if (length(x_names) > 8L)
      x_names <- c(x_names[1:8], "...")
    x_names <- paste(x_names, collapse = ", ")
    gettextf("a data frame with %d rows and %d columns: %s",
      nrow(x), ncol(x), x_names)
  } else if (is.list(x)) {
    gettextf("a list with %d elements", length(x))
  } else if (is.vector(x)) {
    gettextf("a vector of type '%s' and %d elements", typeof(x), length(x))
  } else {
    gettextf("an object of class '%s'", paste(class(x), collapse = "/"))
  }
}
