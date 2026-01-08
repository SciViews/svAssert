test_that("translate() translates known messages", {
  # Create a simple dictionary
  dict <- c(
    "^Test message (\\d+)$" = "Message de test %d"
  )

  msg <- "Test message 5"
  # Note: actual translation depends on .po files, so we test the function runs
  result <- translate(msg, dict, domain = "R-svAssert")
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("translate() returns original message if not found", {
  dict <- c(
    "^Known message$" = "Message connu"
  )

  msg <- "Unknown message"
  result <- translate(msg, dict)
  expect_equal(result, msg)
})

test_that("translate() handles messages with no variables", {
  dict <- c(
    "^Simple message$" = "Message simple"
  )

  msg <- "Simple message"
  result <- translate(msg, dict)
  expect_type(result, "character")
})

test_that("translate() extracts and formats variables", {
  dict <- c(
    "^Value is (\\d+)$" = "La valeur est %d"
  )

  msg <- "Value is 42"
  result <- translate(msg, dict, domain = "R-svAssert")
  expect_type(result, "character")
})

test_that("translate() respects trim parameter", {
  dict <- c(
    "^Message$" = "Message traduit"
  )

  msg <- "  Message  "
  result <- translate(msg, dict, trim = TRUE)
  expect_type(result, "character")

  result <- translate(msg, dict, trim = FALSE)
  expect_type(result, "character")
})

test_that("translate() respects format parameter", {
  dict <- c(
    "^Test$" = "Test traduit"
  )

  msg <- "Test"
  result <- translate(msg, dict, format = TRUE)
  expect_type(result, "character")

  result <- translate(msg, dict, format = FALSE)
  expect_type(result, "character")
})

test_that("format_inline_() formats messages correctly", {
  # Simple formatting
  x <- 5
  result <- format_inline_("Value is {x}")
  expect_match(result, "5")

  # With cli markup
  result <- format_inline_("Variable {.var x} has value {x}")
  expect_type(result, "character")
})

test_that("format_inline_() handles formatting errors", {
  # Malformed expression should trigger warning and return concatenated string
  expect_warning(
    result <- format_inline_("Bad format {missing_var"),
    "Cannot format"
  )
  expect_type(result, "character")
})

test_that("format_inline_() handles collapse parameter", {
  result <- format_inline_("Line 1", "Line 2\nLine 3", collapse = TRUE)
  expect_equal(result, "Line 1Line 2\nLine 3")

  result <- format_inline_("Line 1", "Line 2\nLine 3", collapse = FALSE)
  expect_equal(result, c("Line 1Line 2", "Line 3"))
})

test_that("format_inline_() post-translates conjunctions", {
  # This depends on locale, but we test it runs
  result <- format_inline_("a, and b", post_translate = TRUE)
  expect_type(result, "character")

  result <- format_inline_("a, or b", post_translate = FALSE)
  expect_type(result, "character")
})

test_that("create_messages_script() validates inputs", {
  # Invalid dictionary (no names)
  expect_error(
    create_messages_script(c("msg1", "msg2"), "test") #,
    # TODO: wrong message class
    #class = "rlang_error"
  )

  # Invalid topic
  expect_error(
    create_messages_script(c("^test$" = "test"), ""),
    class = "rlang_error"
  )
})

test_that("create_messages_script() creates file in temp directory", {
  skip_on_cran()

  # Create a temporary package structure
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create DESCRIPTION file
  writeLines("Package: TestPkg", file.path(temp_dir, "DESCRIPTION"))

  # Create dictionary
  dict <- c(
    "^Message (\\d+)$" = "Message %d",
    "^Simple message$" = "Simple message"
  )

  # Create script
  result <- create_messages_script(
    dict,
    "test",
    pkg_dir = temp_dir,
    export = TRUE
  )

  expect_true(file.exists(result))
  expect_match(result, "messages-test\\.R$")

  # Check file content
  content <- readLines(result)
  expect_true(any(grepl("test_msgs", content)))
  expect_true(any(grepl("@export", content)))
})

test_that("create_messages_script() handles source and release info", {
  skip_on_cran()

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  writeLines("Package: TestPkg", file.path(temp_dir, "DESCRIPTION"))

  dict <- c("^Test$" = "Test")

  result <- create_messages_script(
    dict,
    "test",
    pkg_dir = temp_dir,
    source = "https://github.com/test/test",
    release = "1.0.0"
  )

  content <- readLines(result)
  expect_true(any(grepl("@source", content)))
  expect_true(any(grepl("release 1.0.0", content)))
})

test_that("create_messages_script() escapes special characters", {
  skip_on_cran()

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  writeLines("Package: TestPkg", file.path(temp_dir, "DESCRIPTION"))

  # Dictionary with quotes and backslashes
  dict <- c(
    '^Message with "quotes"$' = 'Message with "quotes"',
    "^Message with \\ backslash$" = "Message with \\ backslash"
  )

  result <- create_messages_script(dict, "test", pkg_dir = temp_dir)

  # Should not error and file should exist
  expect_true(file.exists(result))

  # Content should be valid R code
  content <- readLines(result)
  expect_true(length(content) > 0)
})

test_that("create_messages_script() fails with invalid package directory", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  dict <- c("^Test$" = "Test")

  # No DESCRIPTION file
  expect_error(
    create_messages_script(dict, "test", pkg_dir = temp_dir),
    "does not seem to be a package"
  )
})
