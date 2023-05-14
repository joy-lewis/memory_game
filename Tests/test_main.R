# this is a unit test file

# set target class to be tested
source("../main.R", chdir = TRUE)
library(testthat)

# to avoid waringings when running tests
Sys.setenv(LANG = "en_US.UTF-8")

# test: init_player (n_real, n_computer)
test_that("valid player", {
  expect_true(init_player(2, 3) <= (2 + 3))
  expect_true(init_player(7, 0) <= (7 + 0))
  expect_true(init_player(1, 0) <= (1 + 0))
})