# The is_xxx() functions are like checkmate::test_xxx() functions, but they
# record the message in checkmate$message for possible further use.

# This is what I am supposed to do ... but it results in a function that takes
# a little longer to run on small objects than when I directly use .Call()
#.op <- new.env()
#check_numeric <- checkmate::check_numeric
#is_numeric2 <- function(x, lower = -Inf, upper = Inf,
#  finite = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL,
#  min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE,
#  names = NULL, typed.missing = FALSE, null.ok = FALSE) {
#
#  is.logical(.op$message <- check_numeric(x, lower, upper, finite,
#    any.missing, all.missing, len, min.len, max.len, unique, sorted, names,
#    typed.missing, null.ok))
#}

# To avoid one unnecessary function call, I could directly include C code from
# checkmate... but dangerous if c_check_mumeric changes and not allowed on CRAN
#c_check_numeric <- getNativeSymbolInfo('c_check_numeric', 'checkmate')
#is_numeric <- function(x, lower = -Inf, upper = Inf,
#    finite = FALSE, any.missing = TRUE, all.missing = TRUE, len = NULL,
#    min.len = NULL, max.len = NULL, unique = FALSE, sorted = FALSE,
#    names = NULL, typed.missing = FALSE, null.ok = FALSE) {
#
#  is.logical(.op$message <- .Call(c_check_numeric, x, lower, upper, finite,
#    any.missing, all.missing, len, min.len, max.len, unique, sorted, names,
#.   typed.missing, null.ok))
#}

# This is a safe way to do this, providing the body of a checkmate check_xxx()
# function only contains a {.Call(...)} statement: the C code and parameters
# can change, and the function remains valid (but the doc must be adapted in
# case of parameters change).
# Transform a checkmate check_xxx() function into an svAssert is_xxx() one
.check_to_is_function <- function(check_function) {
  body_check <- body(check_function)
  # Verification it is like {.Call(...)}
  if (length(body_check) != 2 || as.character(body_check[[1]]) != "{" ||
      as.character(body_check[[2]][[1]]) != ".Call")
    stop_(
      "`check_function` is not an appropriate checkmate `check_...` function.")

  # This is the new body template where we will inject the .Call() statement
  body_template <- body(function(x) {
    is.logical(checkmate$message <- identity())
  })
  # Inject the .Call()
  body_template[[2]][[2]][[3]] <- body_check[[2]]
  # Create the new is_ function
  is_function <- check_function
  body(is_function) <- body_template
  is_function
}

# Benchmarking the two versions of is_numeric()
#is_numeric <- .check_to_is_function(check_numeric)
#bench::mark(
#  is_numeric(1:10, lower = 0),
#  is_numeric2(1:10, lower = 0)
#)

# Get a message that was set by our is_xxx() functions in checkmate
.checkmate_message <- function() {
  msg <- getNamespace('checkmate')$checkmate$message
  if (!is.null(msg))
    msg <- translate(msg, get('checkmate_msgs'))
  msg
}

# See also in zzz.R for a finalizer


# Translation of checkmate messages ---------------------------------------

# # Comment everything below this line, except to update the checkmate messages
# #
# # Retrieve all messages from {checkmate} source code files and rework them into
# # "matching_regex" = gettext("original or patched message") into a character
# # vector named checkmate_msgs that is used for translation
# # Note: uses curl + awk and works on Unix-like systems (Linux, MacOS)
# # It must be run manually to synchronize with a new {checkmate} version
# #
# # You must run this from the svAssert root directory and it overwrites
# # R/messages-checkmate.R
#
# .chkm_list <- list(
#   repo     = "https://github.com/mllg/checkmate",
#   release  = "v2.3.3",
#   topic    = "checkmate",
#   msgs     = character(0)
# )
# .chkm_list$get_checkmate_messages <- function(repo, release, topic) {
#
#   checkmate_dir <- file.path(tempdir(check = TRUE), topic)
#   dir.create(checkmate_dir)
#   odir <- setwd(checkmate_dir)
#
#   # Download a release from the checkmate GitHub repository
#   release_url <- sprintf("%s/archive/refs/tags/%s.zip", repo, release)
#   zip_file <- "checkmate.zip"
#   res <- system2("curl", c("-L", release_url, "--output", zip_file))
#   if (res != 0)
#     stop("Downloading checkmate release failed.")
#
#   # Unzip the release
#   unzip(zip_file)
#   unlink(zip_file)
#   unzipped_dir <- list.dirs(".", recursive = FALSE)
#   if (length(unzipped_dir) != 1L)
#     stop("Unzipping checkmate release failed.")
#   src_dir <- file.path(unzipped_dir, "src")
#
#   # Extract messages from relevant C source files
#   msgs <- c(
#     system(paste("awk -v RS='\"' '!(NR%2)'", file.path(src_dir, "qassert.c")),
#       intern = TRUE),
#     system(paste("awk -v RS='\"' '!(NR%2)'", file.path(src_dir, "checks.c")),
#       intern = TRUE))
#
#   # We don't need the unzipped files anymore
#   setwd(odir)
#   unlink(checkmate_dir, recursive = TRUE)
#
#   # Filter out false positives (must start with un uppercase and contain at
#   # least one space)
#   msgs <- sort(unique(msgs[grep("^[A-Z].* ", msgs)]), decreasing = TRUE)
#   # Note: sorted in decreasing order, so that, e.g., "Incorrect %s, in %s" is
#   # captured despite "Incorrect %s" also exists
#   # (in v2.3.3, there are 83 messages with %i, %g, or %s / '%s' + one %c)
#   # Replace the '%c' format code by '%s'
#   msgs <- gsub("'%c'", "'%s'", msgs, fixed = TRUE)
#
#   # Compute the regular expressions that identify the messages
#   # Escape problematic characters: . + * ? ^ $ {} []
#   rexs <- gsub("([.+*?^${}[\\]])", "\\\\\\1", msgs, perl = TRUE)
#   # translate() searches for () to identify variables, so we must replace () in
#   # the messages themselves by .
#   rexs <- gsub("(", ".", gsub(")", ".", rexs, fixed = TRUE), fixed = TRUE)
#   # %i is integer -> replace with ([0-9]+)
#   # %g is numeric -> replace with (-?[0-9]+\\.?[0-9]*[eE]?[+-]?[0-9]*)
#   # '%s' is string surrounded by quotes -> replace with '([^']*)'
#   # %s is string -> replace with (.+)
#   # => compute regexes to extract the variables
#   rexs <- gsub("%i", "([0-9]+)",
#     gsub("%g", "(-?[0-9]+\\.?[0-9]*E?[-+]?[0-9]*)",
#       gsub("%s", "(.+)", gsub("'%s'", "'([^']*)'", rexs))))
#   # Indicate we must match the whole string with ^...$
#   rexs <- paste0("^", rexs, "$")
#
#   # Now that regular expressions are computed, one can slightly adapt messages,
#   # for instance, to include cli formats, as soon as the %s, %i and %g code
#   # remain in the same order in the modified messages
#   #typos:
#   msgs <- gsub("chararacters", "characters", msgs, fixed = TRUE)
#   # (-)Inf, NA, NaN and NULL
#   msgs <- gsub("(-?Inf)", "{.code \\1}", msgs, perl = TRUE)
#   msgs <- gsub("NA", "{.code NA}", msgs, fixed = TRUE)
#   msgs <- gsub("NaN", "{.code NaN}", msgs, fixed = TRUE)
#   msgs <- gsub("'NULL'", "{.code NULL}", msgs, fixed = TRUE)
#   # Arguments
#   msgs <- gsub("rgument '([a-zA-Z_.]+)'", "rgument {.arg \\1}", msgs,
#     perl = TRUE)
#   msgs[msgs == "Timezones of 'x' and 'upper' must match"] <-
#     "Timezones of {.arg x} and {.arg upper} must match"
#   msgs[msgs == "Timezones of 'x' and 'lower' must match"] <-
#     "Timezones of {.arg x} and {.arg lower} must match"
#   # Variables names
#   msgs <- gsub("ariable '%s'", "ariable {.var %s}", msgs, fixed = TRUE)
#   # Fields
#   msgs <- gsub("olumn '%s'", "olumn {.field %s}", msgs, fixed = TRUE)
#   # Classes
#   msgs <- sub("class identifier '%s'", "class identifier {.cls %s}", msgs,
#     fixed = TRUE)
#   msgs <- sub("class '%s', not '%s'", "class {.cls %s}, not {.cls %s}", msgs,
#     fixed = TRUE)
#   msgs <- gsub("POSIXct", "{.cls POSIXct}", msgs, fixed = TRUE)
#   # Types (warning: do NOT use .type here, it is NOT what you are looking for)
#   # One incorrect type (should be option)
#   msgs <- sub("Unknown type '%s'", "Unknown option '%s'", msgs, fixed = TRUE)
#   msgs <- gsub("(type '.+, not) '%s'", "\\1 {.cls %s}", msgs,
#     perl = TRUE)
#   msgs <- gsub("type '%s'", "type {.cls %s}", msgs, fixed = TRUE)
#   # Add a dot at the end of sentences, if missing
#   msgs <- sub("([^.!?])$", "\\1.", msgs)
#
#   # Our final object has rexs as names
#   names(msgs) <- rexs
#
#   msgs
# }
#
# # Do not run in the package, only when updating messages manually!
# .chkm_list$msgs <- .chkm_list$get_checkmate_messages(repo = .chkm_list$repo,
#   release = .chkm_list$release, topic = .chkm_list$topic)
# # Inspect your messages visually to check they are correct..
# .chkm_list$msgs
# # ... then, write messages into R/messages-checkmate.R
# create_messages_script(.chkm_list$msgs, .chkm_list$topic,
#   source = .chkm_list$repo, release = .chkm_list$release)
# # Clean up
# rm(.chkm_list)
