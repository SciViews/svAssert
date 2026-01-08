test_that("stop_greater_than() generates error for basic comparisons", {
  x <- 1
  
  expect_error(
    stop_greater_than(x, 2),
    class = "rlang_error"
  )
})

test_that("stop_greater_than() handles vector comparisons with modifiers", {
  x <- c(1, 2, 3)
  y <- c(2, 3, 4)
  
  # All modifier
  expect_error(
    stop_greater_than(x, y, mod = "all"),
    class = "rlang_error"
  )
  
  # Any modifier
  expect_error(
    stop_greater_than(x, y, mod = "any"),
    class = "rlang_error"
  )
})

test_that("stop_> is an alias for stop_greater_than", {
  expect_error(
    `stop_>`(1, 2),
    class = "rlang_error"
  )
})

test_that("stop_less_than() generates error for basic comparisons", {
  x <- 2
  
  expect_error(
    stop_less_than(x, 1),
    class = "rlang_error"
  )
})

test_that("stop_less_than() handles vector comparisons", {
  x <- c(3, 4, 5)
  y <- c(2, 3, 4)
  
  expect_error(
    stop_less_than(x, y, mod = "all"),
    class = "rlang_error"
  )
})

test_that("stop_< is an alias for stop_less_than", {
  expect_error(
    `stop_<`(2, 1),
    class = "rlang_error"
  )
})

test_that("comparison stops handle NA values", {
  x <- c(1, NA, 3)
  y <- c(2, 2, 2)
  
  # greater_than with NA
  expect_error(
    stop_greater_than(x, y),
    class = "rlang_error"
  )
  
  expect_error(
    stop_greater_than(x, y, na.rm = TRUE),
    class = "rlang_error"
  )
  
  # less_than with NA
  expect_error(
    stop_less_than(y, x),
    class = "rlang_error"
  )
})

test_that("comparison stops validate mod parameter", {
  expect_error(
    stop_greater_than(1, 2, mod = "invalid"),
    "Invalid.*mod"
  )
  
  expect_error(
    stop_less_than(1, 2, mod = "invalid"),
    "Invalid.*mod"
  )
})

test_that("comparison stops handle custom messages", {
  expect_error(
    stop_greater_than(1, 2, par. = list(
      msg = "Custom message"
    )),
    "Custom message"
  )
  
  expect_error(
    stop_less_than(2, 1, par. = list(
      footer = c("*" = "Footer note")
    )),
    class = "rlang_error"
  )
})

test_that("comparison stops work with function calls", {
  expect_error(
    stop_greater_than(length(1:3), 5),
    class = "rlang_error"
  )
  
  expect_error(
    stop_less_than(nrow(data.frame(x = 1:5)), 3),
    class = "rlang_error"
  )
})

test_that("comparison stops handle edge cases", {
  # NULL values
  expect_error(
    stop_greater_than(NULL, 1),
    class = "rlang_error"
  )
  
  expect_error(
    stop_less_than(NULL, 1),
    class = "rlang_error"
  )
  
  # Empty vectors
  expect_error(
    stop_greater_than(numeric(0), 1),
    class = "rlang_error"
  )
})
