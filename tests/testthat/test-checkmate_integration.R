test_that("checkmate integration functions are available", {
  # Check internal functions exist
  expect_true(exists(".check_to_is_function", 
                     envir = asNamespace("svAssert"), 
                     inherits = FALSE))
  expect_true(exists(".checkmate_message", 
                     envir = asNamespace("svAssert"), 
                     inherits = FALSE))
})

test_that("is_numeric is properly created from checkmate", {
  # is_numeric should be a function
  expect_true(is.function(is_numeric))
  
  # Should have the same formals as checkmate::check_numeric
  check_formals <- names(formals(checkmate::check_numeric))
  is_formals <- names(formals(is_numeric))
  expect_equal(is_formals, check_formals)
})

test_that("is_numeric sets checkmate$message on failure", {
  # Get access to internal function
  get_msg <- svAssert:::.checkmate_message
  
  # Test that should fail
  result <- is_numeric("not numeric")
  expect_false(result)
  
  # Message should be set
  msg <- get_msg()
  expect_type(msg, "character")
  expect_true(nchar(msg) > 0)
})

test_that("is_numeric returns TRUE for valid inputs", {
  expect_true(is_numeric(1))
  expect_true(is_numeric(1:10))
  expect_true(is_numeric(c(1.5, 2.5)))
  expect_true(is_numeric(integer(0)))
})

test_that("is_numeric returns FALSE for invalid inputs", {
  expect_false(is_numeric("a"))
  expect_false(is_numeric(list(1, 2)))
  expect_false(is_numeric(factor(1:3)))
})

test_that("checkmate message is translated", {
  get_msg <- svAssert:::.checkmate_message
  
  # Trigger a checkmate message
  result <- is_numeric("text")
  expect_false(result)
  
  # Get the translated message
  msg <- get_msg()
  
  # Should be a character string
  expect_type(msg, "character")
  expect_true(nchar(msg) > 0)
  
  # Should contain information about the type error
  expect_match(msg, "character|numeric|type", ignore.case = TRUE)
})

test_that("checkmate_msgs dictionary exists", {
  # The checkmate_msgs should be exported or accessible
  expect_true(exists("checkmate_msgs", 
                     where = asNamespace("svAssert")))
  
  # It should be a character vector
  msgs <- get("checkmate_msgs", envir = asNamespace("svAssert"))
  expect_type(msgs, "character")
  
  # Should have names (the regex patterns)
  expect_true(length(names(msgs)) > 0)
  expect_equal(length(names(msgs)), length(msgs))
})

test_that("checkmate messages have proper regex patterns", {
  msgs <- get("checkmate_msgs", envir = asNamespace("svAssert"))
  
  # Names should be regex patterns (start with ^)
  regex_names <- names(msgs)
  expect_true(all(startsWith(regex_names, "^")))
  
  # Names should end with $
  expect_true(all(endsWith(regex_names, "$")))
})

test_that("integration: is_numeric with stop_is_numeric", {
  test_fun <- function(x) {
    is_numeric(x, len = 1, lower = 0) || stop_is_numeric(x)
    sqrt(x)
  }
  
  # Valid input
  expect_equal(test_fun(4), 2)
  
  # Invalid: wrong type
  expect_error(test_fun("text"), class = "rlang_error")
  
  # Invalid: wrong length
  expect_error(test_fun(1:5), class = "rlang_error")
  
  # Invalid: negative value
  expect_error(test_fun(-1), class = "rlang_error")
})

test_that("other checkmate functions can be converted", {
  # Test the conversion function with check_numeric
  convert_fn <- svAssert:::.check_to_is_function
  
  # Should work with check_numeric
  is_num_test <- convert_fn(checkmate::check_numeric)
  expect_true(is.function(is_num_test))
  
  # Should return logical
  expect_true(is.logical(is_num_test(1)))
  expect_true(is.logical(is_num_test("a")))
})

test_that("checkmate integration handles all parameter combinations", {
  # Test various parameter combinations
  x <- c(1, 2, 3, 4, 5)
  
  # Length constraints
  expect_true(is_numeric(x, len = 5))
  expect_false(is_numeric(x, len = 3))
  
  # Bounds
  expect_true(is_numeric(x, lower = 0, upper = 10))
  expect_false(is_numeric(x, lower = 2))
  
  # Missing values
  x_na <- c(1, NA, 3)
  expect_true(is_numeric(x_na, any.missing = TRUE))
  expect_false(is_numeric(x_na, any.missing = FALSE))
  
  # Finite
  x_inf <- c(1, Inf, 3)
  expect_true(is_numeric(x_inf, finite = FALSE))
  expect_false(is_numeric(x_inf, finite = TRUE))
  
  # Unique
  x_dup <- c(1, 2, 2, 3)
  expect_true(is_numeric(x_dup, unique = FALSE))
  expect_false(is_numeric(x_dup, unique = TRUE))
  
  # Sorted
  x_sorted <- 1:5
  x_unsorted <- c(3, 1, 2)
  expect_true(is_numeric(x_sorted, sorted = TRUE))
  expect_false(is_numeric(x_unsorted, sorted = TRUE))
})

test_that("checkmate messages are properly formatted", {
  get_msg <- svAssert:::.checkmate_message
  
  # Wrong type
  is_numeric("text")
  msg <- get_msg()
  expect_type(msg, "character")
  
  # Wrong length
  is_numeric(1:5, len = 1)
  msg <- get_msg()
  expect_type(msg, "character")
  expect_match(msg, "length|element", ignore.case = TRUE)
  
  # Out of bounds
  is_numeric(10, upper = 5)
  msg <- get_msg()
  expect_type(msg, "character")
  
  # Not unique
  is_numeric(c(1, 1, 2), unique = TRUE)
  msg <- get_msg()
  expect_type(msg, "character")
})
