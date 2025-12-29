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
    msg <- translate(get('checkmate_msgs'), msg)
  msg
}

