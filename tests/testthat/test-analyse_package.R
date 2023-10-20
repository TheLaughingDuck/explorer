# General checks
test_that("bad input is caught",{
  expect_error(analyse_package(156))
})

# T
test_that("this package analyses itself correctly",{
  output <- analyse_package(r"(C:\Users\jorst\Documents\STUDIER\Master Courses\Advanced R Programming\explorer)")

  expect_true(nrow(output) == 1)
  expect_true(output$File[1] == "analyse_package.R")
  expect_true(output$Name[1] == "analyse_package")
})
