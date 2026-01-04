.onLoad <- function(lib, pkg) {# nocov start
  # This is necessary to reset rlang_backtrace_on_error option
  cur_error_handler <- getOption("error")
  if (is.null(cur_error_handler)) {
    options(error = quote((function() {
      options(rlang_backtrace_on_error =
          getOption("svAssert_backtrace_on_error"))
    })()))
  } else if (is.call(cur_error_handler)) {
    error_fun <- function() {
      options(rlang_backtrace_on_error =
          getOption("svAssert_backtrace_on_error"))
      identity(1)
    }
    body(error_fun)[[3]] <- cur_error_handler
    options(error = error_fun)
    rm(error_fun)
  }
  rm(cur_error_handler)
}

# This does not work here, or in .onLoad() because in a call stack
#globalCallingHandlers(error = function(e) {
#  if (all(class(e) != "svAssert_error"))
#    options(rlang_backtrace_on_error =
#        getOption("svAssert_backtrace_on_error"))
#  e
#})

# Internal options
# TODO: change this!
#' @export
.op <- new.env()
.op$verbose <- FALSE

# We use our own stop_() and warning_(), but renamed
#warning <- warning_
stop <- stop_ # nocov end

# Construct a code indicating what case we have in a comparison (==, <, ...)
# It is `mod_lx_x_ly` where
# - mod is 'one', 'any' or 'all',
# - x represents '==', '!=', '>', '<', '>=', or '<='
# - lx and ly are lengths of operands being '0', '1' or 'm' for many)
# - In the cases 1_x_m, m_x_1, or _m_x_m with any/all, just use 'mod_multi'
# plus:
# - for 1_x_1, append '_NA' if condition is NA, or _T if condition is TRUE
#  For any or all with na.rm == TRUE and NA value, also append '_rm'
# - for multiple and any/all, distinguish 3 cases related to NA:
#   1) NA values and na.rm == FALSE: append '_NA'
#   2) all values are NA and na.rm == TRUE: append '_NA_rm_all'
#   3) other cases (no NA or some NA + na.rm == TRUE): append '_T' if cond T
# For _T, only if test_it = TRUE (otherwise, we trust, ans consider it is FALSE)
# Return a list with code, length_x, length_y, nas, and no_nas
.case_comparison <- function(x = NULL, y = NULL, rel = "==", mod = "",
  na.rm = FALSE, test_it = TRUE) {

  # Main part of the code
  if (is.null(mod) || mod == "") mod <- "one"
  lx <- length_x <- length(x)
  if (lx > 1) lx <- "m"
  ly <- length_y <- length(y)
  if (ly > 1) ly <- "m"
  if ((mod != "one" && lx != 0 && ly != 0 && (lx == "m" || ly == "m"))) {
    code <- paste(mod, "multi", sep = "_")
    multi <- TRUE
  } else {
    code <- paste(mod, lx, "x", ly, sep = "_")
    multi <- FALSE
  }

  # Number of missing and non missing values in x rel y
  is_na <- suppressWarnings(is.na(x) | is.na(y))
  nas <- sum(is_na)
  no_nas <- sum(!is_na)
  if (lx == 1 && ly == 1) {# single value
    if (nas > 0) {
      code <- paste0(code, "_NA")
      if (mod != "one" && isTRUE(na.rm))
        code <- paste0(code, "_rm")
    } else if (isTRUE(test_it)) {# not NA, do we test for TRUE?
      if (suppressWarnings(do.call(rel, list(x, y))))
        code <- paste0(code, "_T")
    }

  } else if (multi == TRUE) {# multiple + any/all
    if (nas > 0 && isFALSE(na.rm)) {
      code <- paste0(code, "_NA")
    } else if (no_nas == 0) {# na.rm == TRUE, but all are NA
      code <- paste0(code, "_NA_rm_all")
    } else if (isTRUE(test_it)) {# some NA values with na.rm = TRUE, or no NA
      tests <- suppressWarnings(do.call(rel, list(x, y)))
      if (mod == "any" && any(tests, na.rm = TRUE)) {
        code <- paste0(code, "_T")
      } else if (all(tests, na.rm = TRUE)) {
        code <- paste0(code, "_T")
      }
    }
  }

  if (.op$verbose) message("Comparison case code: ", code)

  list(code = code, length_x = length_x, length_y = length_y,  nas = nas,
    no_nas = no_nas)
}

# Build a contextual comparison error message given the case, the rel(ational)
# operator and the two arguments' names
.case_comparison_message <- function(case, rel, arg, arg2, arg_name, arg2_name, x, y,
  fun, mod) {

  x_name <- as.character(substitute(x))
  y_name <- as.character(substitute(y))

  # Construct the error message, depending on the case
  msg <- switch(case$code,

    # Got one or two length 0 logical -> not OK for one() or any()
    one_0_x_0 = c(
      '!' = gettextf("Both {.code %s} and {.code %s} have length 0.",
        arg, arg2),
      'i' = gettext("Must have length 1."),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = NULL)),
    one_0_x_1 = ,
    one_0_x_m = c(
      '!' = gettextf("{.code %s} has length 0.", arg),
      'i' = gettext("Must have length 1."),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = NULL)),
    one_1_x_0 = ,
    one_m_x_0 = c(
      '!' = gettextf("{.code %s} has length 0.", arg2),
      'i' = gettext("Must have length 1."),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = NULL)),

    any_0_x_0 = c(
      '!' = gettextf("Both {.code %s} and {.code %s} have length 0.",
        arg, arg2),
      'i' = gettext("Must have length >= 1."),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = NULL)),
    any_0_x_1 = ,
    any_0_x_m = c(
      '!' = gettextf("{.code %s} has length 0.", arg),
      'i' = gettext("Must have length >= 1."),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = NULL)),
    any_1_x_0 = ,
    any_m_x_0 = c(
      '!' = gettextf("{.code %s} has length 0.", arg2),
      'i' = gettext("Must have length >= 1."),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = NULL)),

    # Got one or two length 0 logical -> should be OK for all()
    all_0_x_0 = c(
      '!' = gettextf("Both {.code %s} and {.code %s} have length 0.",
        arg, arg2),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = c('i' =
          gettextf("Should be OK, but {.fun %s} called anyway.", fun),
          .internal = TRUE))),
    all_0_x_1 = ,
    all_0_x_m = c(
      '!' = gettextf("{.code %s} has length 0.", arg),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = c('i' =
            gettextf("Should be OK, but {.fun %s} called anyway.", fun),
          .internal = TRUE))),
    all_1_x_0 = ,
    all_m_x_0 = c(
      '!' = gettextf("{.code %s} has length 0.", arg2),
      # Check if something like x == NULL -> is.null(x)
      .case_null_in_comparison_message(arg, arg2, arg_name,
        arg2_name, x, y, fun, rel, mod, else_msg = c('i' =
            gettextf("Should be OK, but {.fun %s} called anyway.", fun),
          .internal = TRUE))),

    # We have both sides being length one. any() or all() have no effect here
    # but if it is a missing value, depends on na.rm=TRUE/FALSE for any()/all()
    one_1_x_1 = ,
    any_1_x_1 = ,
    all_1_x_1 = c(# TODO: different message if we got one or two constants
      '!' = gettextf("{.code %s} must be %s {.code %s}.",
        arg, rel, arg2),
      'i' = if (is.language(arg_name)) # If not a constant, show its value
        gettextf("{.code %s} is: {.val {%s}}.", arg, x_name) else NULL,
      'i' = if (is.language(arg2_name)) # If not a constant, show its value
        gettextf("{.code %s} is: {.val {%s}}.", arg2, y_name) else NULL,
      'x' = if (!is.language(arg_name) && !is.language(arg2_name))
        gettext("It is silly to compare two constants, may be check you code.")
          else NULL),

    one_1_x_1_T = ,
    any_1_x_1_T = ,
    all_1_x_1_T = c(
      '!' = gettextf("{.code %s} %s {.code %s}.", arg, rel, arg2),
      'i' = gettextf("Should be OK, but {.fun %s} called anyway.", fun),
      .internal = TRUE),

    one_1_x_1_NA = ,
    any_1_x_1_NA = ,
    all_1_x_1_NA = c(# Cannot have NA in these cases
      '!' = gettext("Missing value where TRUE / FALSE is required."),
      # This one deals with constant NA/NaN values in comparisons
      # x == NA for is.na(x) and the like
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = c(
          'i' = gettextf("{.code %s} is {.code NA}.",
            if (is.na(x)) arg else arg2)))),

    any_1_x_1_NA_rm = c(# any(NA, na.rm = TRUE) is FALSE
      '!' = gettextf("Missing value in {.code %s} not allowed.",
        if (is.na(x)) arg else arg2),
      # Deal with constant NA/NaN values in comparisons
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = NULL)),
    all_1_x_1_NA_rm = c(# all(NA, na.rm = TRUE) is TRUE
      '!' = gettextf("Missing value in {.code %s}.",
        if (is.na(x)) arg else arg2),
      # Deal with constant NA/NaN values in comparisons
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = c('i' =
          gettextf("Should be OK, but {.fun %s} called anyway.", fun),
          .internal = TRUE))),

    # At least one side has length > 1, one is always wrong
    one_1_x_m = c(
      '!' = gettextf("Both {.code %s} and {.code %s} must have length 1.",
        arg, arg2),
      'i' = gettextf("Length of the second one: %i.", case$length_y),
      # Deal with constant NA/NaN values in comparisons
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = NULL)),
    one_m_x_1 = c(
      '!' = gettextf("Both {.code %s} and {.code %s} must have length 1.",
        arg, arg2),
      'i' = gettextf("Length of the first one: %i.", case$length_x),
      # Deal with constant NA/NaN values in comparisons
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = NULL)),
    one_m_x_m = c(
      '!' = gettextf("Both {.code %s} and {.code %s} must have length 1.",
        arg, arg2),
      'i' = gettextf("Length of the first one: %i.", case$length_x),
      'i' = gettextf("Length of the second one: %i.", case$length_y)),

    # any()/all(), we can have multiple comparisons, result also depends on NAs
    # - NA with na.rm=FALSE -> not allowed
    # - all NA with na.rm=TRUE -> any() gives FALSE, while all() gives TRUE
    # - some NA with na.rm=TRUE or no NA -> any()/all() depend on non-NA values
    any_multi = c(
      '!' = gettextf("Not any {.code %s} is %s {.code %s}.",
        arg, rel, arg2),
      'i' = gettextf("Must have at least one %s.", rel)),
    all_multi = c(
      '!' = gettextf("Not all {.code %s} are %s {.code %s}.",
        arg, rel, arg2),
      'i' = gettextf("Must all be %s.", rel)),

    any_multi_NA = ,
    all_multi_NA = c(
      '!' = gettextf(
        "{%s} missing value{?s} when comparing {.code %s} with {.code %s}.",
        "case$nas", arg, arg2),
      # Deal with constant NA/NaN values in comparisons
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = c('i' =
            gettext("May not have missing values.")))),

    any_multi_NA_rm_all = c(
      '!' = gettextf(
        "All values are missing when comparing {.code %s} with {.code %s}.",
        arg, arg2),
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = c('i' =
          gettext("Must have at least one non missing value.")))),
    all_multi_NA_rm_all = c(
      '!' = gettextf(
        "All values are missing when comparing {.code %s} with {.code %s}.",
        arg, arg2),
      .case_na_in_comparison_message(arg, arg2, arg_name, arg2_name, x, y, fun,
        rel, mod, else_msg = c('i' =
          gettextf("Should be OK, but {.fun %s} called anyway.", fun),
          .internal = TRUE))),

    any_multi_T = c(
      '!' = gettextf("At least one is TRUE in {.code %s} %s {.code %s}.",
        arg, rel, arg2),
      'i' = gettextf("Should be OK, but {.fun %s} called anyway.", fun),
      .internal = TRUE),
    all_multi_T = c(
      '!' = gettextf("All are TRUE in {.code %s} %s {.code %s}.",
        arg, rel, arg2),
      'i' = gettextf("Should be OK, but {.fun %s} called anyway.", fun),
      .internal = TRUE),

    # Default case: should not happen
    ""
  )

  # Extract .internal flag as an attribute
  .internal <- FALSE
  if (any(names(msg) == ".internal")) {
    .internal <- TRUE
    msg <- msg[names(msg) != ".internal"]
  }
  attr(msg, ".internal") <- isTRUE(.internal)

  msg
}

# In a comparison, if one or both are constant NA or NaN, (or NA_integer_, etc.)
# Tel to use is.na(), is.nan(), anyNA(), allNA(), ...
.case_na_in_comparison_message <- function(arg, arg2, arg_name, arg2_name,
  x, y, fun, rel, mod, else_msg) {

  # Check if one or both arguments are constant NA/NaN
  arg_na <- deparse(arg_name) %in% c("NA", "NA_integer_", "NA_real_",
    "NA_character_", "NA_complex_", "NaN")
  arg2_na <- deparse(arg2_name) %in% c("NA", "NA_integer_", "NA_real_",
    "NA_character_", "NA_complex_", "NaN")

  if ((arg_na || arg2_na) && rel != "==") {# Silly x > NA
    c('x' = gettextf("Revise: {.code %s %s %s} is a nonsense.",
      arg, rel, arg2))
  } else if (arg_na && arg2_na) { # Silly again NA == NA
    c('x' = gettextf("Revise: {.code %s %s %s} is a nonsense.",
      arg, rel, arg2))
  } else if (arg_na) {# First arg is NA or NaN == x
    if (deparse(arg_name) == "NaN") {
      c('i' = gettextf(
        "Did you mean {.code %s(%s)} instead of {.code %s == %s}?",
          "is.nan", arg2, "NaN", arg2))
    } else if (mod == "any") {# any(NA == x) -> anyNA(x)
      c('i' = gettextf
        ("Did you mean {.code anyNA(%s)} instead of {.code any(%s == %s)}?",
          arg2, arg, arg2))
    } else {# NA == x -> is.na(x)
      c('i' = gettextf
        ("Did you mean {.code %s(%s)} instead of {.code %s == %s}?",
          "is.na", arg2, arg, arg2))
    }
  } else if (arg2_na) {
    if (deparse(arg2_name) == "NaN") {
      c('i' = gettextf(
        "Did you mean {.code %s(%s)} instead of {.code %s == %s}?",
        "is.nan", arg, arg, "NaN"))
    } else if (mod == "any") {# any(x == NA) -> anyNA(x)
      c('i' = gettextf
        ("Did you mean {.code anyNA(%s)} instead of {.code any(%s == %s)}?",
          arg, arg, arg2))
    } else {# x == NA -> is.na(x)
      c('i' = gettextf
        ("Did you mean {.code %s(%s)} instead of {.code %s == %s}?",
          "is.na", arg, arg, arg2))
    }
  } else {# Just one or both arguments contain NA/NaN values
    else_msg
  }
}

# In a comparison, something like x == NULL should be replace by is.null(x)
.case_null_in_comparison_message <- function(arg, arg2, arg_name, arg2_name,
  x, y, fun, rel, mod, else_msg) {

  # Check if one or both arguments are constant NULL
  arg_null <- deparse(arg_name) == "NULL"
  arg2_null <- deparse(arg2_name) == "NULL"

  if ((arg_null || arg2_null) && rel != "==") { # Silly x > NULL
    c('x' = gettextf("Revise: {.code %s %s %s} is a nonsense.",
      arg, rel, arg2))
  } else if (arg_null && arg2_null) { # Silly NULL == NULL
    c('x' = gettextf("Revise: {.code %s %s %s} is a nonsense.",
      arg, rel, arg2))
  } else if (arg_null) {# First arg is NULL
    c('i' = gettextf("Did you mean {.code %s(%s)} instead of {.code %s %s %s}?",
        "is.null", arg2, arg, rel, arg2))
  } else if (arg2_null) {
    c('i' = gettextf("Did you mean {.code %s(%s)} instead of {.code %s %s %s}?",
        "is.null", arg, arg, rel, arg2))
  } else {# Just one or both arguments contain NULL values
    else_msg
  }
}

# This is rlang::check_required(), but modified for translatable errors
#check_required <- function(x) {
#  if (missing(x))
#    stop("{.arg {substitute(x)}} is absent but must be supplied.",
#      class = "missing_argument", call = stop_top_call(2L))
#  invisible(TRUE)
#}

# Get the name of the first argument of a calling function
# (used to compute the subcall of an error class with error_class())
# No, not used for now
#.get_first_arg_name <- function(which = -1L) {
#  fun <- sys.function(which)
#  if (is.null(fun)) {
#    NULL
#  }  else {
#    fun_names <- names(formals(fun))
#    if (length(fun_names)) fun_names[[1]] else NULL
#  }
#}
