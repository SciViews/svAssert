test_that("stop_() generates proper error messages", {
  # Basic error message
  expect_error(
    stop_("This is an error"),
    "This is an error"
  )

  # Error with bullet points
  expect_error(
    stop_("Main error", i = "Info message", x = "Problem detail"),
    "Main error"
  )

  # Error with glue interpolation
  n <- 5
  expect_error(
    stop_("Found {n} elements"),
    "Found 5 elements"
  )
})

test_that("stop_() handles formatting errors gracefully", {
  # Malformed glue expression should still produce an error,
  # but associated with a warning
  expect_warning(
    stop_("Error with {missing_var")
  ) |>
  expect_error(class = "rlang_error")
})

test_that("error_class() computes proper class names", {
  test_fun <- function() {
    error_class()
  }

  result <- test_fun()
  expect_true(grepl("test_fun.*error", result))

  # With class_id
  result <- error_class(class_id = "custom")
  expect_true(grepl("custom", result))
})

test_that("stop_top_call() finds the correct call", {
  inner_fun <- function() {
    stop_top_call(1L)
  }
  outer_fun <- function() {
    .__top_call__. <- TRUE
    list(top_call = inner_fun(), current = environment())
  }
  result <- outer_fun()
  expect_equal(result$top_call, result$current)
})

test_that("stop_top_call() finds the parent call", {
  inner_fun <- function() {
    stop_top_call(1L)
  }
  outer_fun <- function() {
    .__top_call__. <- FALSE
    list(top_call = inner_fun(), current = environment())
  }
  caller <- function() {
    c(outer_fun(), list(caller = environment()))
  }
  result <- caller()
  expect_equal(result$top_call, result$caller)
})


test_that("object_info() describes objects correctly", {
  # NULL
  expect_match(object_info(NULL), "NULL")

  # Vector
  expect_match(object_info(1:5), "vector")
  expect_match(object_info(letters), "vector")

  # List
  x <- list(a = 1, b = 2, c = 3)
  info <- object_info(x)
  expect_match(info, "list")
  expect_match(info, "3 element")

  # Data frame
  df <- data.frame(x = 1:3, y = 4:6)
  info <- object_info(df)
  expect_match(info, "data frame")
  expect_match(info, "3 row")
  expect_match(info, "2 column")
})

test_that("lbl() deparsing works correctly", {
  # Simple expression
  expect_equal(lbl(quote(x)), "x")

  # Function call
  expect_match(lbl(quote(mean(x))), "mean")

  # Long expression gets truncated
  long_expr <- quote(very_long_function_name_that_exceeds_width(arg1, arg2))
  result <- lbl(long_expr, width = 20)
  expect_true(nchar(result) <= 24) # width + "..."
  expect_match(result, "\\.\\.\\.") # Contains "..."
})
