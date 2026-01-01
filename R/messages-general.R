#' General messages for translation
#'
#' Messages and regular expressions required to translate general error using
#' [translate()].
#'
#' @format ## `general_msgs`
#' A list with two components:
#' \describe{
#'   \item{msgs}{Messages to translate}
#'   \item{rexs}{Corresponding regular expressions to extract variables from
#'     messages}
#' }
"general_msgs"

# Checkmate messages for translation
if (FALSE) {
  # all.equal.numeric() do no translate these:
  gettext("Mean absolute difference: %g")
  gettext("Mean scaled difference: %g")
  gettext("Mean relative difference: %g")
}

# general_msgs <- list(
#   msgs = c(
#     "Mean absolute difference: %g",
#     "Mean scaled difference: %g",
#     "Mean relative difference: %g"
#   ),
#   rexs = c(
#     "^Mean absolute difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$",
#     "^Mean scaled difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$",
#     "^Mean relative difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$"
#   )
# )
# usethis::use_data(general_msgs, overwrite = TRUE)
