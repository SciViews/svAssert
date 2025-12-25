#' Translate a message given a set of known messages and regex patterns
#'
#' @param x A list with two elements: msgs (character vector of known messages)
#' and rexs (character vector of regex patterns corresponding to msgs)
#' @param msg The message to be translated
#'
#' @returns The translated message if a correspondence is found and a
#' translation is known, or the original message otherwise.
#' @export
#'
#' @examples
#' data(checkmate_msgs) # Translations for {checkmate} messages
#' msg <- "Must be of type 'numeric', not 'character'"
#' translate(checkmate_msgs, msg)
translate <- function(x, msg) {
  # Then, translate the message and use sprintf() to reinject the variables
  # Now try to identify the message
  # Restrict search to the messages starting with the same two letters
  # TODO: special treatment if a %x code in the beginning, or second place
  matching <- which(startsWith(x$msgs, substring(msg, 1L, 2L)))
  if (!length(matching)) # No candidate, just return the original message
    return(msg)

  found <- 0
  for (i in matching) {
    if (grepl(x$rexs[i], msg)) {
      found <- i
      break
    }
  }
  if (!found) # No match found, just return the original message
    return(msg)
  # Translate message
  tr <- gettext(x$msgs[found], domain = "R-svAssert", trim = TRUE)
  if (tr == x$msgs[found]) # No translation, just return the original message
    return(msg)

  # Extract variables
  numvars <- sum(gregexpr("(", x$rexs[i], msg, fixed = TRUE)[[1]] > 0)
  if (numvars == 0) # No variables, just return the translated string
    return(tr)
  # Extract variables
  strs <- sub(x$rexs[found], paste0("\\", 1:numvars, collapse = "&&&"), msg)
  # Separate strings
  vars <- as.list(strsplit(strs, "&&&", fixed = TRUE)[[1]])
  # Try to convert into a number
  vars <- lapply(vars, function(x) {
    val <- suppressWarnings(as.numeric(x))
    if (is.na(val)) x else val
  })
  do.call(sprintf, c(list(tr), vars))
}
