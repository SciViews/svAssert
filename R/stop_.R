#' Enhanced stop
#'
#' @description

#' `stop_()` is an enhanced version of [stop()] to generate meaningful error
#' with error-recoverable glue interpolation and translation.
#' `error_class()`, `stop_top_call()`, `object_info()` and `lbl()`` are support
#' functions that help building contextual and explicit error messages.
#'
#' @details
#' `stop_()` is a wrapper around [rlang::abort()] that provides more control
#' on the stop message thanks to [cli::cli_abort()] glue interpolation and
#' [gettext()] translation. It can recover from errors in the formatting
#' processes. In this case, it throws the raw error message with a
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
#' You can use [rlang::global_entrace()] to display classical base R [stop()]
#' messages in a similar way as `stop_()` and [rlang::abort()] do. You can also
#' use [rlang::last_error()] to revisit the last error message, or
#' [rlang::last_trace()] to inspect the backtrace of the message.
#' Finally, the display of the error message is customisable. See
#' [Customising condition messages](
#' https://rlang.r-lib.org/reference/topic-condition-customisation.html).
#'
#' @param ... One or more character strings with the error or warning
#'   messages to be translated. Name them `'*' =`, `'i' =`, `'v' =`, `'x' =` or
#'   `'!' =` to format a bullet-list with the message items. First message item
#'   is considered to use the `'!'` bullet by default. The messages also support
#'   glue interpolation and inline markups, see [Formatting messages with cli](
#'   https://rlang.r-lib.org/reference/topic-condition-formatting.html)
#'   and [cli::format_inline()].
#' @param domain The translation. domain, see [gettext()]. If `NA` or `""`,
#'   messages are not translated (use this with messages that are already
#'   translated).
#' @param class The subclass of the error condition message. By default, it is
#'   computed by `error_class()`, using the name of the function in `call`
#'   (plus, optionally, the `class_id`), or `"svAssert_error"` by default.
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
#' @returns `stop_()` is invoked for its side-effects, to stop execution of the
#'   current code.
#' @export
#' @seealso [rlang::abort()], [cli::cli_abort()], [gettext()] [stop()]
#'
#' @examples
#' # If you want to include the error messages in the translation strings in
#' # your package, you have to rename `stop_()` into `stop()` because
#' # [tools::xgettext2pot()] will only pick up messages in the later ones.
#' library(svAssert)
#' stop <- stop_
#'
#' # Note: the |> try() are there to catch error. Do not use them in your code!
#' # Correctly formatted stop messages
#' n <- "some text"
#' stop("{.var n} must be a numeric vector",
#'   x = "You've supplied a {.cls {class(n)}} vector.") |> try()
#'
#' # Incorrectly formatted stop messages (error in glue formatting: missing
#' # second closing `\}` in `\{.cls\{class(n)\}`
#' stop("{.var n} must be a numeric vector",
#'   x = "You've supplied a {.cls {class(n)} vector.") |> try()
#'
#' # Automatic pluralisation
#' n <- 1:18
#' stop("{.var n} must be a scalar numeric:",
#'   i = "There {?is/are} {length(n)} element{?s}.",
#'   x = "Provide a single numeric, not {object_info(n)}.") |> try()
#'
#' # When issued from within a function, the function call is indicated
#' test1 <- function(x) {
#'   stop("{.var x} must be a scalar numeric:",
#'     i = "There {?is/are} {length(x)} element{?s} in {.var x}.")
#' }
#' test1(1:3) |> try()
#'
#' # If another function calls `test1()`, error is still reported from test1:
#'
#' test2 <- function(x) {
#'   test1(x)
#' }
#' test2(1:3) |> try()
#'
#' # In such a case, it is better to report the error from `test2()`.
#' # You can do that by stating `._top_call_. <- TRUE` in the body of `test2()`.
#' test2 <- function(x) {
#'   .__top_call__. <- TRUE
#'   test1(x)
#' }
#' test2(1:3) |> try()
#'
#' # When you design an helper function (a function that is always called from
#' # another function), you can set `.__top_call__. <- FALSE` to force pointing
#' # to the calling function. In this case, .__top_call___. <- TRUE is not
#' # needed in the calling function.
#' stop_is_scalar_numeric <- function(x) {
#'   .__top_call__. <- FALSE
#'   stop("{.var {lbl(substitute(x))}} must be a scalar numeric")
#' }
#' test3 <- function(y) {# A function with a proper assertion on y
#'   (is.numeric(y) && length(y) == 1L) || stop_is_scalar_numeric(y)
#'
#'   # Do something with y here...
#' }
#' test3(1:4) |> try()
#'
#' # If test3() is called by another function, test4(), focus depends on the
#' # presence of .__top_call__. in test4()
#' test4 <- function(y) test3(y)
#' test4(1:4) |> try()
#' # or:
#' test4 <- function(y) {
#'   .__top_call__. <- TRUE
#'   test3(y)
#' }
#' test4(1:4) |> try()
#' rm(stop)
stop_ <- function(..., domain = NULL, class = error_class(call,
    class_id = class_id), class_id = .op$class_id, call = NULL,
    parent = NULL, .inherit = TRUE, .internal = FALSE, .file = NULL,
    .envir = parent.frame(), .frame = .envir, .trace_bottom = NULL,
    .last_call = sys.call(-1L)) {

  # Default values for call and class
  if (is.null(call))
    call <- stop_top_call(2L)
  if (is.null(class))
    class <- error_class(call = call, class_id = class_id)

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
    warning(
      "Formatting of the error message failed, using unformatted messages.",
      "\n", message_formatted, call. = FALSE)
    class <- "cli_format_error" # Special class superseding the one provided
    message_formatted <- message
    .internal = TRUE
  }

  # Message for the internal error (same one as in abort(), but translatable)
  if (isTRUE(.internal))
    message_formatted <- c(message_formatted, i = gettext(
      "This is an internal error, please report it to the package authors."))

  # Use my custom backtrace message reminder (for translation)
  #if ("svAssert_error" %in% class) {
    bt_op <- getOption("svAssert_backtrace_on_error", NULL)
    if ((is.null(bt_op) && interactive()) || bt_op == "reminder") {
      message_formatted <- c(message_formatted, format_inline(gettext(
        "Run `{.run rlang::last_trace()}` to see where the error occurred.")))
      # Avoid displaying it twice (silent rlang version)
      options(rlang_backtrace_on_error = "none")
    }
  #}

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

# .signal_stop_ <- function(cnd, file = NULL) {
#   .__signal_frame__. <- TRUE
#   if (is_true(peek_option("rlang::::force_unhandled_error"))) {
#     fallback <- cnd
#   } else {
#     signalCondition(cnd)
#     fallback <- cnd
#     class(fallback) <- c("svAssert_error", "rlang_error", "condition")
#     fallback$message <- ""
#     fallback$rlang$internal$entraced <- TRUE
#   }
#   # Already done in rlang::signal_abort()
#   #poke_last_error(cnd)
#   if (.peek_show_error_messages()) {
#     #cnd <- cnd_set_backtrace_on_error(cnd, peek_backtrace_on_error())
#     # Translate Error (in) (at) : + Caused by
#     trans <- paste0("\\1", gettext(error_ = "Error", in_ = "in", at_ = "at",
#       caused_by_ = "Caused by error in"), "\\2")
#     names(trans) <- c("error_", "in_", "at_", "caused_by_")
#     msg <- cnd_message(cnd, inherit = TRUE, prefix = TRUE)
#     msg <- sub("^(.+Error.+)in(.+)", trans[['in_']], msg, perl = TRUE)
#     msg <- sub("^(.+Error.+)at(.+)", trans[['at_']], msg, perl = TRUE)
#     msg <- sub("^(.+)Error([^:]+:.+)", trans[['error_']], msg, perl = TRUE)
#     msg <- sub("^(.+)Caused by error in(.+)", trans[['caused_by_']], msg,
#       perl = TRUE)
#     .cat_line(msg, file = file %||% .default_message_file())
#   }
#   local_options(show.error.messages = FALSE)
#   base::stop(cnd) #stop(fallback)
# }
#
# # Unexported functions from rlang needed for .signal_stop_()
# # rlang:::.peek_show_error_messages()
# .peek_show_error_messages <- function() {
#   !is_false(peek_option("show.error.messages"))
# }
#
# # rlang:::cat_line()
# .cat_line <- function(..., .trailing = TRUE, file = "") {
#   cat(.paste_line(..., .trailing = .trailing), file = file)
# }
#
# # rlang:::paste_line()
# .paste_line <- function(..., .trailing = FALSE) {
#   text <- .chr(...)
#   if (.trailing) {
#     paste0(text, "\n", collapse = "")
#   } else {
#     paste(text, collapse = "\n")
#   }
# }
#
# # rlang:::chr()
# .chr <- rlang:::chr
#
# # rlang:::default_message_file()
# .default_message_file <- function() {
#   opt <- peek_option("rlang:::message_file")
#   if (!is_null(opt)) {
#     return(opt)
#   }
#   if ((is_interactive() || .is_rstudio()) && sink.number("output") ==
#       0 && sink.number("message") == 2) {
#     stdout()
#   }
#   else {
#     stderr()
#   }
# }
#
# # rlang:::is_rstudio()
# .is_rstudio <- function() {
#   Sys.getenv("RSTUDIO_SESSION_PID") %in% c(Sys.getpid(), .getppid())
# }
#
# # Get parent pid, not exported from rlang
# .getppid <- rlang:::getppid
# # Or, using package {ps}
# #.getppid <- function() {
# #  ps_ppid(ps_handle())
# #}

# TODO: I need to find a better way, because warning_()..., call. = TRUE
# returns warning in warning_(...) -> not nice
# @rdname stop_
# @export
# @param call. Logical, whether to include the call in the warning message.
# @param immediate. Logical, whether to issue the warning immediately even if
# `getOption("warn") <= 0`. Note that this is not respected for condition
# objects.
# @param noBreaks. logical, indicating as far as possible that the message
#   should be output as a single line when `options(warn = 1)`.
#warning_ <- function(..., call. = FALSE, immediate. = FALSE, noBreaks. = FALSE,
#    domain = NULL) {
#  base::warning(..., call. = call., immediate. = immediate.,
#    noBreaks. = noBreaks., domain = domain)
#}

#' @rdname stop_
#' @export
#' @param nframe The number of frames to go up the call stack to start finding
#'   the top call (as soon as `.__to_call__.` is found in the environment,
#'   look in its parent frame).
#' @returns
#' `stop_top_call()` returns the top call to be used for stop condition messages
#' (to be used as `call` argument of `stop_()`).
#' call for stop condition messages.
stop_top_call <- function(nframe = 1L) {
  env <- call <- parent.frame(nframe)
  max_frames <- sys.nframe()
  if (max_frames > nframe) {
    for (i in (nframe + 1):max_frames) {
      call <- env
      env <- parent.frame(i)
      if (is.null(env$`.__top_call__.`))
        if (!isFALSE(call$`.__top_call__.`))
          break
    }
  }
  call
}

#' @rdname stop_
#' @export
#' @returns
#' `error_class()` returns a character string with the error class name computed
#' for `stop_()`, that is, "fun_id_error", or "svAssert_error", by default.
error_class <- function(call = parent.frame(), class_id = NULL) {
  # Get frame number corresponding to this environment
  idx <- which(vapply(sys.frames(), identical, logical(1), call))
  if (length(idx)) {
    fun <- deparse(sys.call(idx)[[1]])
  } else {
    fun <- NULL
  }

  class_error <- paste(c(fun, class_id, "error"), collapse = '_')
  if (class_error == "error") # When both fun and class_id are NULL
    class_error <- "svAssert_error"

  class_error
}

#' @rdname stop_
#' @param x An R object to describe.
#' @export
#' @returns
#' `object_info()` returns a character string describing the R object provided.
object_info <- function(x) {
  # TODO: more variants (matrix, array, factor/ordered, Date, POSIXct, ...) with
  # short but informative description
  # TODO: name of columns for list too + name of slots for S4/S7
  if (is.null(x)) {
    "'NULL'"
  } else if (is.data.frame(x)) {
    x_names <- paste0("'", names(x), "'")
    if (length(x_names) > 3L)
      x_names <- c(x_names[1:3], "...")
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

#' @rdname stop_
#' @param expr An R expression to deparse
#' @param width Maximum width of the deparsed expression
#' @param nlines Maximum number of lines for the deparsed expression
#' @export
#' @returns `lbl()` returns a deparsed version of an expression suitable for
#' \{<format> \{lbl(x)\}\} where `<format>` could be `.var`, `.arg`, or `.code`.
lbl <- function(expr, width = 30L, nlines = 1L) {
  res <- deparse1(expr, width.cutoff = width + 3L, nlines = nlines)
  if (nchar(res) > width)
    res <- paste(substr(res, 1L, width - 4L), "...")
  res
}
