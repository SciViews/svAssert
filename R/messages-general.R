#' Messages for Translation: general
#'
#' Messages and regular expressions required to identify error strings
#' to translate using [svAssert::translate()] for 'general'.
#'
#' @format ## `general_msgs`
#' A character vector with messages and corresponding regular
#' expressions as names.
#' @export
general_msgs <- (\() {
  gettext <- c # Do not translate now, just catch messages in .po files
  c(
  "^Mean absolute difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$" =
    gettext("Mean absolute difference: %g"),
  "^Mean scaled difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$" =
    gettext("Mean scaled difference: %g"),
  "^Mean relative difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$" =
    gettext("Mean relative difference: %g")
  )
})()
