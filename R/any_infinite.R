#' Check if an object contains infinite values
#'
#' @description
#' Supported are atomic types (see \code{\link[base]{is.atomic}}), lists and data frames.
#'
#' @param x An object to check for the presence of infinite values.
#' @return `logical(1)` Returns `TRUE` if any element is `-Inf` or `Inf`.
#' @useDynLib svAssert c_any_infinite
#' @export
#' @examples
#' any_infinite(1:10)
#' any_infinite(c(1:10, Inf))
#' data(iris, package = "datasets")
#' iris[3, 3] <- Inf
#' any_infinite(iris)
any_infinite <- function(x) {
  .Call(c_any_infinite, x)
}