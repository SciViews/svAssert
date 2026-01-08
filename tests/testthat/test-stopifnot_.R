test_that("stopifnot_() works with simple assertions", {
  # These should not error
  expect_silent(stopifnot_(TRUE))
  expect_silent(stopifnot_(1 == 1))
  expect_silent(stopifnot_(1 == 1, 2 == 2))
  
  # These should error
  expect_error(stopifnot_(FALSE))
  expect_error(stopifnot_(1 == 2))
  expect_error(stopifnot_(TRUE, FALSE))
})

test_that("stopifnot_() works with vectors", {
  # All TRUE
  expect_silent(stopifnot_(c(TRUE, TRUE, TRUE)))
  
  # Contains FALSE
  expect_error(stopifnot_(c(TRUE, FALSE, TRUE)))
  
  # Contains NA
  expect_error(stopifnot_(c(TRUE, NA, TRUE)))
})

test_that("stopifnot_() handles all.equal() calls", {
  # Should pass
  expect_silent(stopifnot_(all.equal(1, 1)))
  expect_silent(stopifnot_(all.equal(pi, 3.14159265, tolerance = 1e-7)))
  
  # Should fail with informative message
  expect_error(
    stopifnot_(all.equal(pi, 3.14)),
    "not equal"
  )
})

test_that("stopifnot_() works with length checks", {
  x <- 1:5
  
  # Should pass
  expect_silent(stopifnot_(length(x) == 5))
  expect_silent(stopifnot_(length(x) > 0))
  
  # Should fail
  expect_error(stopifnot_(length(x) == 1))
  expect_error(stopifnot_(length(x) < 3))
})

test_that("get_stop_fun() identifies stop functions correctly", {
  # Test with length comparison
  result <- get_stop_fun(length(x) == 1, call_it = FALSE, force_stop = FALSE)
  expect_equal(result$stop_fun, "stop_==")
  expect_equal(result$mod, "")
  
  # Test with negation
  result <- get_stop_fun(length(x) != 1, call_it = FALSE, force_stop = FALSE)
  expect_equal(result$mod, "!")
  
  # Test with any()
  result <- get_stop_fun(any(x > 0), call_it = FALSE, force_stop = FALSE)
  expect_equal(result$mod, "any")
  
  # Test with all()
  result <- get_stop_fun(all(x > 0), call_it = FALSE, force_stop = FALSE)
  expect_equal(result$mod, "all")
})

test_that("mod_not() correctly identifies negation", {
  expect_true(mod_not("!"))
  expect_true(mod_not("!any"))
  expect_true(mod_not("!all"))
  
  expect_false(mod_not(""))
  expect_false(mod_not("any"))
  expect_false(mod_not("all"))
  expect_false(mod_not(NULL))
})

test_that("modifier simplification works correctly", {
  # Access internal function
  simplify <- svAssert:::.simplify_modifier
  
  expect_equal(simplify(""), "")
  expect_equal(simplify("!"), "!")
  expect_equal(simplify("any"), "any")
  expect_equal(simplify("all"), "all")
  
  # Double negation
  expect_equal(simplify("!!"), "")
  expect_equal(simplify("!!any"), "any")
  
  # Complex cases
  expect_equal(simplify("all!"), "!any")
  expect_equal(simplify("any!"), "!all")
  expect_equal(simplify("!all!"), "any")
  expect_equal(simplify("!any!"), "all")
  
  # Multiple any/all
  expect_equal(simplify("anyanyany"), "any")
  expect_equal(simplify("allallall"), "all")
})

test_that("core expression extraction works", {
  # Access internal function
  extract <- svAssert:::.extract_core_expression
  
  # Simple negation
  result <- extract(quote(!x), "")
  expect_equal(result$mod, "!")
  expect_equal(result$core_expr, quote(x))
  
  # any() wrapper
  result <- extract(quote(any(x)), "")
  expect_equal(result$mod, "any")
  expect_equal(result$core_expr, quote(x))
  
  # all() wrapper
  result <- extract(quote(all(x)), "")
  expect_equal(result$mod, "all")
  expect_equal(result$core_expr, quote(x))
  
  # != transformed to ==
  result <- extract(quote(x != 5), "")
  expect_equal(result$mod, "!")
  expect_equal(result$core_expr, quote(x == 5))
  
  # <= transformed to >
  result <- extract(quote(x <= 5), "")
  expect_equal(result$mod, "!")
  expect_equal(result$core_expr, quote(x > 5))
  
  # >= transformed to <
  result <- extract(quote(x >= 5), "")
  expect_equal(result$mod, "!")
  expect_equal(result$core_expr, quote(x < 5))
})

test_that("mod_content() generates appropriate messages", {
  # When value equals expression, no message
  expect_null(mod_content(5, 5))
  
  # When they differ
  msg <- mod_content(5, quote(x))
  expect_type(msg, "character")
  expect_match(msg, "is")
  
  # With length() call
  msg <- mod_content(3, quote(length(x)))
  expect_match(msg, "length")
})
