test_that("The .po translation files are up to date", {
  skip_on_cran()
  skip_on_ci()
  # Update .po and .mo files (only test in the source package, not R CMD check)
  if (file.exists("../../DESCRIPTION")) {# This is the source of the package
    cat("\nCompiling .po files...\n")
    res <- try(tools::update_pkg_po("../.."), silent = TRUE)
    expect_false(inherits(res, "try-error"),
      "Updating .po files failed. Run tools::update_pkg_po() manually to debug.")
  }
})
