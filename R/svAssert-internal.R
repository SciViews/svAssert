onLoad <- function(lib, pkg) {# nocov start
  # ...
}# nocov end

# Internal options
.op <- new.env()
.op$verbose <- FALSE

# We use our own stop_() and warning_(), but renamed
stop <- stop_
warning <- warning_

# Transform a checkmate check_xxx() function into an svAssert is_xxx() one
.check_to_is_function <- function(check_function) {
  body_check <- body(check_function)
  # Verification it is like {.Call(...)}
  if (length(body_check) != 2 || as.character(body_check[[1]]) != "{" ||
      as.character(body_check[[2]][[1]]) != ".Call")
    stop("The provided check_function does not seem to be a checkmate check_ function.")

  # This is the new body template where we will inject the .Call() statement
  body_template <- body(function(x) {
    is.logical(checkmate$svAssert_msg <- identity())
  })
  # Inject the .Call()
  body_template[[2]][[2]][[3]] <- body_check[[2]]
  # Create the new is_ function
  is_function <- check_function
  body(is_function) <- body_template
  is_function
}

# Get a message that was set by ours is_xxx() functions in checkmate
.checkmate_message <- function() {
  msg <- getNamespace('checkmate')$checkmate$svAssert_msg
  if (!is.null(msg))
    msg <- translate(get('checkmate_msgs'), msg)
  msg
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

#library(checkmate)
#c_check_numeric <- getNativeSymbolInfo('c_check_numeric', 'checkmate')
