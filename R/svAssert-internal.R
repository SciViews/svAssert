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
# TODo: change this!
#' @export
.op <- new.env()
.op$verbose <- FALSE

# We use our own stop_() and warning_(), but renamed
stop <- stop_
warning <- warning_ # nocov end

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
