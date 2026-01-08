test_that("stop_greater_or_equal() generates error for basic comparisons", {
  x <- 2
  
  expect_error(
    stop_greater_or_equal(x, 5),
    class = "rlang_error"
  )
})

test_that("stop_greater_or_equal() handles vector comparisons", {
  x <- c(1, 2, 3)
  y <- c(2, 3, 4)
  
  expect_error(
    stop_greater_or_equal(x, y, mod = "all"),
    class = "rlang_error"
  )
  
  expect_error(
    stop_greater_or_equal(x, y, mod = "any"),
    class = "rlang_error"
  )
})

test_that("stop_>= is an alias for stop_greater_or_equal", {
  expect_error(
    `stop_>=`(1, 3),
    class = "rlang_error"
  )
})

test_that("stop_less_or_equal() generates error for basic comparisons", {
  x <- 5
  
  expect_error(
    stop_less_or_equal(x, 2),
    class = "rlang_error"
  )
})

test_that("stop_less_or_equal() handles vector comparisons", {
  x <- c(3, 4, 5)
  y <- c(1, 2, 3)
  
  expect_error(
    stop_less_or_equal(x, y, mod = "all"),
    class = "rlang_error"
  )
})

test_that("stop_<= is an alias for stop_less_or_equal", {
  expect_error(
    `stop_<=`(5, 2),
    class = "rlang_error"
  )
})

test_that("stop_not_equal() generates error for basic comparisons", {
  x <- 5
  
  expect_error(
    stop_not_equal(x, 5),
    class = "rlang_error"
  )
})

test_that("stop_not_equal() handles vector comparisons", {
  x <- c(1, 2, 3)
  y <- c(1, 2, 3)
  
  # With 'all' modifier - all should be different
  expect_error(
    stop_not_equal(x, y, mod = "all"),
    class = "rlang_error"
  )
  
  # With 'any' modifier - at least one should be different
  expect_error(
    stop_not_equal(x, y, mod = "any"),
    class = "rlang_error"
  )
})

test_that("stop_!= is an alias for stop_not_equal", {
  expect_error(
    `stop_!=`(5, 5),
    class = "rlang_error"
  )
})

test_that("inequality stops handle NA values", {
  x <- c(1, NA, 3)
  y <- c(2, 2, 2)
  
  # greater_or_equal with NA
  expect_error(
    stop_greater_or_equal(x, y),
    class = "rlang_error"
  )
  
  expect_error(
    stop_greater_or_equal(x, y, na.rm = TRUE),
    class = "rlang_error"
  )
  
  # less_or_equal with NA
  expect_error(
    stop_less_or_equal(y, x),
    class = "rlang_error"
  )
  
  # not_equal with NA
  y_equal <- c(1, NA, 3)
  expect_error(
    stop_not_equal(x, y_equal),
    class = "rlang_error"
  )
})

test_that("inequality stops validate mod parameter", {
  expect_error(
    stop_greater_or_equal(1, 2, mod = "invalid"),
    "Invalid.*mod"
  )
  
  expect_error(
    stop_less_or_equal(1, 2, mod = "invalid"),
    "Invalid.*mod"
  )
  
  expect_error(
    stop_not_equal(1, 2, mod = "invalid"),
    "Invalid.*mod"
  )
})

test_that("inequality stops handle custom messages", {
  expect_error(
    stop_greater_or_equal(1, 5, par. = list(
      msg = "Custom header"
    )),
    "Custom header"
  )
  
  expect_error(
    stop_less_or_equal(5, 2, par. = list(
      footer = c("*" = "Footer")
    )),
    class = "rlang_error"
  )
  
  expect_error(
    stop_not_equal(5, 5, par. = list(
      msg = "Values should differ"
    )),
    "Values should differ"
  )
})

test_that("inequality stops work with function calls", {
  expect_error(
    stop_greater_or_equal(length(1:3), 5),
    class = "rlang_error"
  )
  
  expect_error(
    stop_less_or_equal(nrow(data.frame(x = 1:10)), 5),
    class = "rlang_error"
  )
  
  expect_error(
    stop_not_equal(sum(c(2, 3)), 5),
    class = "rlang_error"
  )
})

test_that("inequality stops handle edge cases", {
  # NULL values
  expect_error(
    stop_greater_or_equal(NULL, 1),
    class = "rlang_error"
  )
  
  expect_error(
    stop_less_or_equal(NULL, 1),
    class = "rlang_error"
  )
  
  expect_error(
    stop_not_equal(NULL, 1),
    class = "rlang_error"
  )
  
  # Empty vectors
  expect_error(
    stop_greater_or_equal(numeric(0), 1),
    class = "rlang_error"
  )
  
  expect_error(
    stop_less_or_equal(numeric(0), 1),
    class = "rlang_error"
  )
})

test_that("inequality stops with test_it = FALSE", {
  # Should still generate errors even without testing
  expect_error(
    stop_greater_or_equal(1, 5, test_it = FALSE),
    class = "rlang_error"
  )
  
  expect_error(
    stop_less_or_equal(5, 1, test_it = FALSE),
    class = "rlang_error"
  )
  
  expect_error(
    stop_not_equal(5, 5, test_it = FALSE),
    class = "rlang_error"
  )
})

test_that("boundary conditions work correctly", {
  # Edge case: equal values with >=
  expect_error(
    stop_greater_or_equal(3, 5),
    class = "rlang_error"
  )
  
  # Edge case: equal values with <=
  expect_error(
    stop_less_or_equal(5, 3),
    class = "rlang_error"
  )
  
  # Edge case: checking inequality of identical vectors
  x <- c(1, 2, 3)
  expect_error(
    stop_not_equal(x, x, mod = "all"),
    class = "rlang_error"
  )
})
