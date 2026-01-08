# Test Suite for svAssert

This directory contains comprehensive tests for the `svAssert` package.

## Test Files

### Core Functionality Tests

- **test-general.R**: General package tests including:
  - Package loading
  - Function availability checks
  - Integration tests with real-world examples
  - Package options testing

- **test-stop_.R**: Tests for the `stop_()` function and related helpers:
  - Basic error message generation
  - Bullet point formatting
  - Glue interpolation
  - `error_class()` computation
  - `stop_top_call()` call stack navigation
  - `object_info()` object descriptions
  - `lbl()` expression deparsing

- **test-stopifnot_.R**: Tests for assertion functions:
  - `stopifnot_()` with simple and complex assertions
  - Vector assertions
  - `all.equal()` integration
  - `get_stop_fun()` function identification
  - Modifier extraction and simplification
  - `mod_not()` negation detection
  - Core expression extraction

### Validation Tests

- **test-is_numeric.R**: Tests for numeric validation:
  - Basic type checking
  - Length constraints (exact, min, max)
  - Value bounds (lower, upper)
  - Missing value handling
  - Finite value checking
  - Uniqueness checking
  - Sorted value checking
  - NULL handling
  - Name checking
  - Integration with `stop_is_numeric()`

### Comparison Tests

- **test-stop_equal.R**: Tests for equality checks (`==`):
  - Scalar comparisons
  - Vector comparisons with modifiers
  - NA value handling
  - NULL comparisons
  - Custom messages
  - Function call arguments

- **test-comparisons.R**: Tests for strict comparison operators:
  - `stop_greater_than()` and `stop_>` (`>`)
  - `stop_less_than()` and `stop_<` (`<`)
  - Vector comparisons with `all` and `any` modifiers
  - NA value handling
  - Custom messages
  - Edge cases

- **test-inequality_comparisons.R**: Tests for inclusive comparison operators:
  - `stop_greater_or_equal()` and `stop_>=` (`>=`)
  - `stop_less_or_equal()` and `stop_<=` (`<=`)
  - `stop_not_equal()` and `stop_!=` (`!=`)
  - Vector comparisons
  - Boundary conditions
  - NA value handling

### Translation Tests

- **test-translate.R**: Tests for message translation and formatting:
  - `translate()` with known messages
  - Variable extraction and formatting
  - `format_inline_()` with cli markup
  - Error recovery in formatting
  - `create_messages_script()` script generation
  - Special character escaping

- **test-translations.R**: Tests for translation file updates:
  - `.po` file compilation
  - Translation consistency checks

## Running the Tests

### Run All Tests

```r
# From R console
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Run Specific Test File

```r
testthat::test_file("tests/testthat/test-stop_.R")
```

### Run Tests with Coverage

```r
covr::package_coverage()
```

## Test Coverage

The test suite aims for comprehensive coverage of:

1. **Core assertion functions**: `stop_()`, `stopifnot_()`
2. **Comparison operators**: `==`, `!=`, `<`, `>`, `<=`, `>=`
3. **Validation functions**: `is_numeric()` and checkmate integration
4. **Helper functions**: `error_class()`, `object_info()`, `lbl()`, etc.
5. **Translation system**: `translate()`, `format_inline_()`
6. **Modifier system**: `mod_not()`, `get_stop_fun()`
7. **Edge cases**: NULL values, NA values, empty vectors, etc.
8. **Error recovery**: Malformed messages, formatting errors

## Test Conventions

- All tests use `testthat` framework (edition 3)
- Tests that might fail in CRAN checks include `skip_on_cran()`
- Tests that require specific CI setup include `skip_on_ci()`
- Error-generating code is wrapped in `try()` or `expect_error()`
- Integration tests demonstrate real-world usage patterns

## Adding New Tests

When adding new features to svAssert:

1. Create a new test file or add to existing one
2. Follow naming convention: `test-<feature>.R`
3. Group related tests with `test_that()` blocks
4. Include tests for:
   - Normal cases
   - Edge cases (NULL, NA, empty vectors)
   - Error conditions
   - Integration with other functions
5. Document complex test scenarios with comments

## Dependencies

Tests require:

- testthat (>= 3.0.0)
- The package to be loaded or installed
- Optional: covr for coverage reports
