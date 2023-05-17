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

# test: memory (n_row = 4, n_col = 4, n_real = 2, n_computer = 0)
test_that("Full simulation of a game", {
    # Input sequence that shuld produce the output below
    user_inputs <- c(1,1,1,4,1,2,"y",1,3,2,1,"a","y",2,2,1,3,"y",2,1,2,3)

    # Define the expected output
    expected_output <- c(
        "Player 1 starts!",
        "In each move you have to choose two cards.",
        "",
        "Player 1 , choose your first card (1: row, 2: column)!",
        "Player 1 , choose your second card (1: row, 2: column)!",
        "Card not valid. Again: ",
        "Correct, Player 1 plays again! Current Leaderboard:",
        "  Player 1   Player 2 ",
        "         1          0 ",
        "Press [y], when you are ready to move on!",
        "Player 1 , choose your first card (1: row, 2: column)!",
        "Player 1 , choose your second card (1: row, 2: column)!",
        "Wrong, Player 2 plays! Press [y], when you are ready to move on!",
        "Wrong key!Press [y], when you are ready to move on!",
        "Player 2 , choose your first card (1: row, 2: column)!",
        "Player 2 , choose your second card (1: row, 2: column)!",
        "Correct, Player 2 plays again! Current Leaderboard:",
        "  Player 1   Player 2 ",
        "         1          1 ",
        "Press [y], when you are ready to move on!",
        "Player 2 , choose your first card (1: row, 2: column)!",
        "Player 2 , choose your second card (1: row, 2: column)!",
        "Correct, Player 2 wins! Final leaderboard:",
        "  Player 1   Player 2 ",
        "         1          2 "
        )

    # Capture the output of predefined user_inputs sequence
    captured_output <- capture.output({
        memory(n_row=2, n_col=3, n_real = 2, n_computer = 0, user_inputs = user_inputs)
    })

    # Compare the captured output with the expected output
    expect_equal(captured_output, expected_output)
})

#### TEST GAME FOR n_real = 0, n_computer = 2