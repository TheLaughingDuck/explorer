# General checks
test_that("bad input is caught",{
  expect_error(analyse_package(156))
})

# Test specific packages
test_that("this package works on the dplyr package",{
  # Please keep in mind that dplyr changes frequently
  #output <- analyse_package(r"(https://github.com/tidyverse/dplyr/tree/main)")
})
