#' Translate a message given a set of known messages and regex patterns
#'
#' @param msg The message to be translated
#'
#' @param dictionary A character vector with messages to translate and
#'   regular expressions to find the corresponding error strings as names.
#' @param domain The domain where to look for messages (see [gettext()]). The
#'   default looks in "R-svAssert", and it should be changed for translations
#'   that are available in another package.
#' @param trim Logical, indicating whether leading and trailing whitespace
#'   before looking for message translation. `TRUE` by default.
#' @param format Logical, indicating whether to format the translated message
#'   with `format_inline_()`. Default is `TRUE`.
#' @param ... Additional arguments passed to `format_inline_()`.
#'
#' @returns For `translate()` the translated message if a correspondence is
#'   found in the dictionary and a translation is known, or the original
#'   message otherwise.
#' @export
#'
#' @examples
#' checkmate_msgs[1:3] # First 3 {checkmate} messages
#' msg <- "Unknown class identifier 'my_class'"
#' translate(msg, checkmate_msgs, domain = "R-svAssert")
translate <- function(msg, dictionary, domain = "R-svAssert", trim = TRUE,
    format = TRUE, ...) {
  # Translate the message and use sprintf() to reinject the variables
  # Now try to identify the message
  # Restrict search to the messages starting with the same two letters
  # TODO: special treatment if a %x code in the beginning, or second place
  rexs <- names(dictionary)
  msgs <- unname(dictionary)
  matching <- which(startsWith(msgs, substring(msg, 1L, 2L)))
  if (!length(matching)) { # No candidate, just return the original message
    if (isTRUE(format))
      msg <- format_inline_(msg, post_translate = FALSE, ...)
    return(msg)
  }

  found <- 0
  for (i in matching) {
    if (grepl(rexs[i], msg)) {
      found <- i
      break
    }
  }
  if (!found) # No match found, just return the original message
    return(msg)
  # Translate message
  tr <- gettext(msgs[found], domain = domain, trim = trim)
  if (tr == msgs[found]) { # No translation, just return the original message
    if (isTRUE(format))
      msg <- format_inline_(msg, post_translate = FALSE, ...)
    return(msg)
  }

  # Extract variables
  numvars <- sum(gregexpr("(", rexs[i], msg, fixed = TRUE)[[1]] > 0)
  if (numvars == 0) { # No variables, just return the translated string
    if (isTRUE(format))
      tr <- format_inline_(tr, post_translate = TRUE, ...)
    # Is there a finalizer?
    finalizer <- attr(dictionary, "finalizer")
    if (!is.null(finalizer) && is.function(finalizer))
      tr <- finalizer(tr)
    return(tr)
  }
  # Extract variables
  strs <- sub(rexs[found], paste0("\\", 1:numvars, collapse = "&&&"), msg)
  # Separate strings
  vars <- as.list(strsplit(strs, "&&&", fixed = TRUE)[[1]])
  # Try to convert into a number
  vars <- lapply(vars, function(x) {
    val <- suppressWarnings(as.numeric(x))
    if (is.na(val)) x else val
  })
  msg <- do.call(sprintf, c(list(tr), vars))

  # Is there a finalizer?
  finalizer <- attr(dictionary, "finalizer")
  if (!is.null(finalizer) && is.function(finalizer))
    msg <- finalizer(msg)

  # Do we format the message with cli::format_inline()?
  if (isTRUE(format))
    msg <- format_inline_(msg, post_translate = TRUE, ...)

  msg
}

#' @rdname translate
#' @param ... Messages passed to [format_inline()].
#' @param .envir The environment in which to evaluate expressions in `...`.
#' @param collapse Logical, indicating whether to collapse multiple lines into
#'   a single string with newlines.
#' @param keep_whitespace Logical, indicating whether to keep whitespace in
#'   the formatted message.
#' @param post_translate Logical, indicating whether to translate ", and " and
#'   ", or " in the final message.
#' @returns For `format_inline_()` a formatted message as a string. If an error
#'   occurs during formatting, a warning is issued and a simple concatenation of
#'   the arguments is returned instead.
#' @seealso [cli::format_inline()]
#' @export
format_inline_ <- function(..., .envir = parent.frame(), collapse = TRUE,
    keep_whitespace = TRUE, post_translate = TRUE) {

  msg <- try_fetch(format_inline(..., .envir = .envir, collapse = collapse,
    keep_whitespace = keep_whitespace),
    error = function(cnd) {
      warning(gettext(
        "Cannot format error message (signal it to package authors):"),
        # TODO: translation of cli and glue error messages here...
        "\n", conditionMessage(cnd), call. = FALSE)
      NULL
    }
  )

  if (is.null(msg)) {# Problem during formatting of the message
    msg <- paste0(..., collapse = if (isTRUE(collapse)) "\n" else NULL)
  } else if (isTRUE(post_translate)) {
    # Translate ", and " and ", or "
    msg <- gsub(", and ", gettext(", and "), msg, fixed = TRUE)
    msg <- gsub(", or ", gettext(", or "), msg, fixed = TRUE)
  }

  msg
}

#' @rdname translate
#' @param topic The identifier for a dictionary. Usually, it should be the name
#'   of the package that generates the messages, but it is not mandatory. Use
#'   `"general"` for a set with no particular topic.
#' @param pkg_dir The root directory of the sources of an R package. The
#'    produced `messages-topic.R` script file is created in the `R/`
#'    subdirectory.
#' @param export Logical, indicating whether the created object should be
#'   exported. Default is `TRUE`.
#' @param source An optional URL indicating where the messages come from, e.g.,
#'    a GitHub repository.
#' @param release An optional string indicating the release version of the
#'   source where the messages were extracted from.
#' @returns For `create_messages_script()` the name of the script file that is
#'   created is returned invisibly. The function is used for its side effect of
#'   creating the adequate script file.
#' @export
create_messages_script <- function(dictionary, topic, pkg_dir = ".",
    export = TRUE, source = NULL, release = NULL) {

  stopifnot_(is.character(dictionary), length(dictionary) > 0L,
    length(names(dictionary)) == length(dictionary),
    all(nzchar(names(dictionary))),
    is.character(topic), length(topic) == 1L, nzchar(topic))

  # Check that pkg_dir contains a DESCRIPTION file
  if (!file.exists(file.path(pkg_dir, "DESCRIPTION")))
    stop_("{.path pkg_dir} does not seem to be a package directory.")
  # Create the R/ directory if it does not exist
  dir.create(file.path(pkg_dir, "R"), showWarnings = FALSE)
  # Compute the file name and path
  script_file <- file.path(pkg_dir, "R", paste0("messages-", topic, ".R"))

  # Write the msgs into the file
  cat("#' Messages for Translation: ", topic, "\n",
    "#'\n",
    "#' Messages and regular expressions required to identify error strings\n",
    "#' to translate using [svAssert::translate()] for '", topic, "'.\n",
    "#'\n",
    "#' @format ## `", topic, "_msgs`\n",
    "#' A character vector with messages and corresponding regular\n",
    "#' expressions as names.\n",
    sep = "", file = script_file)
  if (isTRUE(export))
    cat("#' @export\n", sep = "", file = script_file, append = TRUE)
  if (!is.null(release) && length(release)) {
    rel_msg <- paste0(" (release ", release[1], ")\n")
  } else {
    rel_msg <- "\n"
  }
  if (!is.null(source) && length(source))
    cat("#' @source <", source[1], ">", rel_msg,
      sep = "", file = script_file, append = TRUE)
  cat(topic, "_msgs <- (\\() {\n",
    "  gettext <- c # Do not translate now, just catch messages in .po files\n",
    "  c(\n",
    sep = "", file = script_file, append = TRUE)
  rexs <- names(dictionary)
  code <- character(length(dictionary))
  quote_and_escape <- function(x)
    gsub('"', '\\\\"', gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE)
  for (i in seq_along(dictionary)) {
    code[i] <- paste0("  \"", quote_and_escape(rexs[i]),
      "\" =\n    gettext(\"", quote_and_escape(dictionary[i]), "\")")
  }
  cat(paste(code, collapse = ",\n"), file = script_file, append = TRUE)
  cat("\n  )\n})()\n", file = script_file, append = TRUE)

  invisible(script_file)
}
