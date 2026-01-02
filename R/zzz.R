# Code to run when the rest is loaded --------------------------------

# Finalize translation of {checkmate} messages
attr(checkmate_msgs, "finalizer") <- function(msg) {
  # This string is a variable and is thus not translated until now
  msg <- gsub(" (or 'NULL')", gettext(" (or {.code NULL})"), msg, fixed = TRUE)

  msg
}
