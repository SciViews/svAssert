# CRAN search - assertion functions

Search for assert, check, test (partial, only package name starting with 'test') and valid on 2025/12/25.

## Packages

-   assert: just `assert()` like `stopifnot()`
-   assertable: assertions for tables inside pipelines
-   assertions: use glue, quite slow
-   assertr: assertions inside pipelines
-   assertthat: Hadley Wickam, lots of inverse dependencies, but complex and slow, 4.5µs for `is.character()` assertion
-   bigassertr: redefines `stop2()`/`warning2()`/`message2()` like `stop(sprintf(...))`, but problem with extracting strings to be translated. Then simple `assert_xxx()` functions with `if (...) stop2(...)`
-   checkarg: lot of argument checking functions, but use a long and complex `checkarg()` function -\> too slow for simple checks.
-   **checkmate**: fast (C code) but versatile and feature-rich. `check_xxx()`/`test_xxx()`/`assert_xxx()`/`expect_xxx()` for each test. `qtest()` and `qassert()` with a DSL for quick and versatile test. Lots of inverse dependencies. Little dependencies (backports + utils.)
-   **checkthat**: `check_that()` function to check in a pipeline. Checking helpers written in a simple way -\> possibly useful stuff there.
-   chk: functions `chk_xxx()`/`vld_xxx()`/`check_xxx()` follows tidyverse style guide for errors. The `vld_xxx()` functions return `TRUE` or `FALSE`. Many `vld_xxx()` functions add un unecessary function call. Exemple: `vld_character <- function(x) is.character(x)`.
-   dataMaid: writing reports of data analyses. Not for assertions.
-   editrules: superseeded by validate and errorlocate.
-   erify: imports glue. `check_xxx()` functions but not optimized for speed and a little bit unnecessary complex to construct the error message.
-   errorlocate: complement to validate, not for assetions.
-   quickcheck: for {testthat}, generate various kind of data for tests unsing `any_xxx()` functions. Not for assertions, but could be useful for tests.
-   pointblank: many dependencies. Haevy validators for datasets. Not adapted for assertions.
-   **precondition**: assertion routines to replace `stopifnot()`. `precondition()`, `postcondition()` (executed when a functioon exits) and `sanity_check()`. Allow to give more information in certain conditions... interesting idea, and use C code and `globalCallingHandlers()`.
-   testdat: validate data in testthat tests or pipelines. Not really designed for assertions.
-   tester: losts of `is_xxx()` or `has_xxx()` functions. Also unnecessarily wrap existing functions like `is_matrix <- function(x) is.matrix(x)`, or something like `is_string <- function(x) if (is.character(x)) TRUE else FALSE` (sic!) }
-   typehint: uses `#| degrees_celsius numeric dim(1) not(NA, NULL)` in a function, plus `check_types()`. I don't know if it is a good idea.
-   validata: use `confirm_xxx()` functions, too many dependencies!
-   validate: DSL for data validation. Define rules with `validator()`, then `confront()` theM. A paper explains the idea: <https://cran.r-project.org/web/packages/validate/vignettes/JSS_3483.pdf>. Too complex to use for assertions. Could be nice for complex checking of datasets. There is even a barplot showing how many rules succeeded or failed.
-   vvauditor: data checking tools, too specific?

## Choice

Basic R functions `if` with various `is.xxx()` and other + `stop()`. `stopifnot()` for quick and dirty (multiple) assertion. {checkmate} functions for more advanced assertions.

## TODO

Explore {checkthat} and {precondition}.
