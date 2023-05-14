# this is a unit test file

# set target class to be tested
source("../src/main.R", chdir = TRUE)
library(testthat)

# to avoid waringings when running tests
Sys.setenv(LANG = "en_US.UTF-8")

# set seed for random number generation
set.seed(1)

# test: init_player (n_real, n_computer)
test_that("valid player", {
    expect_true(init_player(2, 3) <= (2 + 3))
    expect_true(init_player(7, 0) <= (7 + 0))
    expect_true(init_player(1, 0) <= (1 + 0))
})

# test: init_board (n, n_row, n_col)
test_that("correct board dimensions", {
    board <- init_board(12, 3, 4)
    expect_equal(dim(board), c(3, 4, 4))
})

test_that("correct initial values", {
    board <- init_board(12, 3, 4)
    expect_true(all(board[, , 1] >= 1 & board[, , 1] <= 13))
    expect_true(all(board[, , 2] >= 1 & board[, , 2] <= 8))
    expect_true(all(board[, , 3] == 0))
    expect_true(all(board[, , 4] == 0))
})
