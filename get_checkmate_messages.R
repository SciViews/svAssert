# Translation of checkmate messages ---------------------------------------
# Note: look also at getOption('error') to install a hook for translation
# of untranslated messages globally.

# TODO: save the script in data-raw, see usethis::use_data_raw()
# To get checkmate messages, I extract a zipped version of the GitHub repos
# and then, use:
# TODO: automate retrieval and unzipping of the repos, then delete it at the end
#awk -v RS='"' '!(NR%2)' qassert.c
#awk -v RS='"' '!(NR%2)' checks.c
path <- "~/Downloads/checkmate-master/src"
msgs <- c(
  system(paste("awk -v RS='\"' '!(NR%2)'", file.path(path, "qassert.c")),
    intern = TRUE),
  system(paste("awk -v RS='\"' '!(NR%2)'", file.path(path, "checks.c")),
    intern = TRUE))
# Filter out false positives (must start with un uppercase and contain at least one space)
msgs <- sort(unique(msgs[grep("^[A-Z].* ", msgs)]))
msgs <- rev(msgs) # So that, e.g., "Incorrect %s, in %s" is captured despite "Incorrect %s" also exists
# There are 83 messages with %i, %g, or %s / '%s' + one %c
# %i is integer -> replace with ([0-9]+)
# %g is numeric -> replace with (-?[0-9]+\\.?[0-9]*e?[+-]?[0-9]*) (to be checked)
# %s or '%s' is string -> replace with (.+)
# => compute regexes to extract the variables
rexs <- gsub("(", ".", gsub(")", ".", msgs, fixed = TRUE), fixed = TRUE)
rexs <- gsub("[", "\\[", gsub("]", "\\]", rexs, fixed = TRUE), fixed = TRUE)
rexs <- gsub("%i", "([0-9]+)",
  gsub("%g", "(-?[0-9]+\\.?[0-9]*E?[-+]?[0-9]*)",
    gsub("'%s'", "'(.+)'",
      gsub("%s", "(.+)", rexs))))
rexs <- paste0("^", rexs, "$")
# Create an object that contains both msgs and rexs
checkmate_msgs <- list(msgs = msgs, rexs = rexs)
# TODO: save this object in rds format as a dataset of the package
# print all msg surrounded by gettext() somewhere in the code, just for
# tools::update_pkg_po()
print_po_msgs <- function(x, file) {
  cat("# Checkmate messages for translation\n\nif (FALSE) {\n", file = file)
  cat(paste0("  gettext(\"", gsub('"', '\\\\"', x$msgs), "\")\n"), sep = "",
    file = file, append = TRUE)
  cat("}\n", file = file, append = TRUE)
}
print_po_msgs(checkmate_msgs,
  file = "R/messages-checkmate.R")
