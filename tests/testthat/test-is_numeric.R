test_that("is_numeric() validates numeric vectors", {
  # Valid cases
  expect_true(is_numeric(1))
  expect_true(is_numeric(1:10))
  expect_true(is_numeric(c(1.5, 2.5, 3.5)))
  expect_true(is_numeric(integer(0)))
  expect_true(is_numeric(numeric(0)))

  # Invalid cases
  expect_false(is_numeric("a"))
  expect_false(is_numeric(letters))
  expect_false(is_numeric(list(1, 2, 3)))
  expect_false(is_numeric(NULL))
})

test_that("is_numeric() respects length constraints", {
  x <- 1:5

  # Exact length
  expect_true(is_numeric(x, len = 5))
  expect_false(is_numeric(x, len = 3))

  # Minimum length
  expect_true(is_numeric(x, min.len = 3))
  expect_true(is_numeric(x, min.len = 5))
  expect_false(is_numeric(x, min.len = 10))

  # Maximum length
  expect_true(is_numeric(x, max.len = 10))
  expect_true(is_numeric(x, max.len = 5))
  expect_false(is_numeric(x, max.len = 3))

  # Combined min and max
  expect_true(is_numeric(x, min.len = 3, max.len = 10))
  expect_false(is_numeric(x, min.len = 6, max.len = 10))
})

test_that("is_numeric() respects value bounds", {
  x <- c(1, 2, 3, 4, 5)

  # Lower bound
  expect_true(is_numeric(x, lower = 0))
  expect_true(is_numeric(x, lower = 1))
  expect_false(is_numeric(x, lower = 2))

  # Upper bound
  expect_true(is_numeric(x, upper = 10))
  expect_true(is_numeric(x, upper = 5))
  expect_false(is_numeric(x, upper = 4))

  # Both bounds
  expect_true(is_numeric(x, lower = 1, upper = 5))
  expect_false(is_numeric(x, lower = 2, upper = 4))
})

test_that("is_numeric() handles missing values correctly", {
  x <- c(1, NA, 3)

  # By default, NA is allowed
  expect_true(is_numeric(x))

  # Disallow any missing
  expect_false(is_numeric(x, any.missing = FALSE))

  # All missing
  all_na <- c(NA, NA, NA)
  expect_true(is_numeric(all_na))
  expect_false(is_numeric(all_na, all.missing = FALSE))

  # Empty vector has no missing values
  expect_true(is_numeric(numeric(0), any.missing = FALSE))
})

test_that("is_numeric() checks for finite values", {
  # Infinite values
  x <- c(1, Inf, -Inf, 2)
  expect_true(is_numeric(x))
  expect_false(is_numeric(x, finite = TRUE))

  # NaN
  x <- c(1, NaN, 2)
  expect_true(is_numeric(x))
  expect_true(is_numeric(x, finite = TRUE))

  # All finite
  x <- c(1, 2, 3)
  expect_true(is_numeric(x, finite = TRUE))
})

test_that("is_numeric() checks for uniqueness", {
  # Unique values
  expect_true(is_numeric(1:5, unique = TRUE))

  # Duplicate values
  expect_false(is_numeric(c(1, 2, 2, 3), unique = TRUE))

  # With NA
  expect_false(is_numeric(c(1, NA, NA), unique = TRUE))
})

test_that("is_numeric() checks for sorted values", {
  # Sorted
  expect_true(is_numeric(1:5, sorted = TRUE))
  expect_true(is_numeric(c(1, 2, 2, 3), sorted = TRUE))

  # Not sorted
  expect_false(is_numeric(c(3, 1, 2), sorted = TRUE))
  expect_false(is_numeric(c(1, 3, 2), sorted = TRUE))
})

test_that("is_numeric() handles NULL correctly", {
  # By default, NULL is not allowed
  expect_false(is_numeric(NULL))

  # With null.ok = TRUE
  expect_true(is_numeric(NULL, null.ok = TRUE))
})

test_that("is_numeric() checks names", {
  # Unnamed vector
  x <- 1:3
  expect_true(is_numeric(x, names = "unnamed"))
  expect_false(is_numeric(x, names = "named"))

  # Named vector
  x <- c(a = 1, b = 2, c = 3)
  expect_false(is_numeric(x, names = "unnamed"))
  expect_true(is_numeric(x, names = "named"))
  expect_true(is_numeric(x, names = "unique"))

  # Duplicate names
  x <- c(a = 1, a = 2, b = 3)
  expect_true(is_numeric(x, names = "named"))
  expect_false(is_numeric(x, names = "unique"))
})

test_that("stop_is_numeric() generates appropriate error messages", {
  # Simple case
  expect_error(
    stop_is_numeric("not numeric"),
    class = "rlang_error"
  )

  # With custom argument name
  expect_error(
    stop_is_numeric(letters, par. = list(arg = "my_var")),
    "my_var"
  )
})

test_that("is_numeric() integration with stop_is_numeric()", {
  my_fun <- function(x) {
    is_numeric(x, len = 1) || stop_is_numeric(x)
    x * 2
  }

  # Should work
  expect_equal(my_fun(5), 10)

  # Should error
  expect_error(my_fun("a"))
  expect_error(my_fun(1:5))
})
