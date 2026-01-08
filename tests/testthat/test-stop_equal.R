test_that("stop_equal() generates error for scalar comparisons", {
  # Basic scalar comparison
  expect_error(
    stop_equal(1, 2),
    class = "rlang_error"
  )
  
  # With variable names
  x <- 5
  expect_error(
    stop_equal(x, 10),
    class = "rlang_error"
  )
})

test_that("stop_equal() handles vector comparisons with 'all' modifier", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  
  expect_error(
    stop_equal(x, y, mod = "all"),
    class = "rlang_error"
  )
  
  # Same length, some equal
  x <- c(1, 2, 3)
  y <- c(1, 5, 3)
  expect_error(
    stop_equal(x, y, mod = "all"),
    class = "rlang_error"
  )
})

test_that("stop_equal() handles vector comparisons with 'any' modifier", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  
  expect_error(
    stop_equal(x, y, mod = "any"),
    class = "rlang_error"
  )
})

test_that("stop_equal() handles NA values", {
  x <- c(1, NA, 3)
  y <- c(1, 2, 3)
  
  # Without na.rm
  expect_error(
    stop_equal(x, y),
    class = "rlang_error"
  )
  
  # With na.rm
  expect_error(
    stop_equal(x, y, na.rm = TRUE),
    class = "rlang_error"
  )
})

test_that("stop_equal() handles NULL comparisons", {
  expect_error(
    stop_equal(NULL, 1),
    class = "rlang_error"
  )
  
  expect_error(
    stop_equal(1, NULL),
    class = "rlang_error"
  )
})

test_that("stop_equal() handles custom messages", {
  expect_error(
    stop_equal(1, 2, par. = list(
      msg = "Custom header",
      footer = c("*" = "Custom footer")
    )),
    "Custom header"
  )
})

test_that("stop_equal() validates mod parameter", {
  expect_error(
    stop_equal(1, 2, mod = "invalid"),
    "Invalid.*mod"
  )
})

test_that("stop_== is an alias for stop_equal", {
  expect_error(
    `stop_==`(1, 2),
    class = "rlang_error"
  )
})

test_that("stop_equal() handles function calls in arguments", {
  expect_error(
    stop_equal(length(c(1, 2)), 1),
    class = "rlang_error"
  )
})

test_that("stop_equal() with test_it = FALSE", {
  # Should still generate error even without testing
  expect_error(
    stop_equal(1, 2, test_it = FALSE),
    class = "rlang_error"
  )
})
