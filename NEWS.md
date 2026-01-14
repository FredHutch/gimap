# gimap 1.1.2

* Fixed vignettes to properly include pre-built images using `knitr::include_graphics()`
* Improved test skipping pattern to use proper testthat skip functions
* All internet resource accesses fail gracefully with informative messages (CRAN policy compliance)
* Added `"id"` to `utils::globalVariables()` to fix compatibility with dplyr 1.2.0+ (which removed the defunct `dplyr::id()`)

# gimap 1.1.1

* Minor bug fixes and documentation improvements

# gimap 1.1.0

* Adjustments to gimap calculations -- all reps collapsed and linear models done with mean across reps.

# gimap 1.0.3

* Polish how annotation data is saved
* Allow stats to be calculated per rep or with all reps combined

# gimap 1.0.2

* Filter has been refactored
* Bug addressed

# gimap 1.0.1

* This is an initial release of this package!
