## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes in this version

This release addresses CRAN policy compliance for internet resource handling:

* Fixed vignettes to properly include pre-built images using `knitr::include_graphics()` 
  instead of markdown syntax. The image directories (`output_timepoints/` and 
  `output_treatment/`) are now included in the package tarball.

* Improved test skipping pattern to use proper testthat `skip_on_cran()` and 
  `skip()` functions inside `test_that()` blocks, rather than conditional 
  execution at the top level of test files.

* All internet resource accesses (Figshare API, DepMap data downloads) are wrapped 
  in `tryCatch()` and fail gracefully with informative messages when resources 
  are unavailable. Functions return `NULL` or the dataset without additional 
  annotation when network resources cannot be reached.

* Added `"id"` to `utils::globalVariables()` to fix compatibility with upcoming 
  dplyr 1.2.0 which removes the defunct `dplyr::id()` export.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.0
* GitHub Actions (ubuntu-latest): R-release
* win-builder: R-devel
