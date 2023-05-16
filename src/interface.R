# this file provides a interface for the memory game,
# i.e. functions that perform plots and console outputs
source("test_helper.R")


print_board <- function(board, n_row, n_col) {
    # board entrys follow this pattern:
    #     pch, symbols from 1 to 13
    #     col, colors from 1 to 8

    ### Plot an empty grid with n_row rows and n_col columns
    plot(0, 0, type = "n", xlim=c(0.8, n_col+0.2), ylim=c(0.8, n_row+0.2), xlab = "", ylab = "", axes = FALSE, bty = "o")
    title("Memory", font.main = 2, line = 3)

    # add x-axis with column numbers
    axis(side = 1, at = 1:n_col, labels = FALSE, tick = FALSE, line = -0.5)
    # add y-axis with row numbers
    axis(side = 2, at = 1:n_row, labels = FALSE, tick = FALSE, line = -0.5)

    # add column labels to top margin of plot
    mtext(1:n_col, side = 3, at = 1:n_col, line = 0.5, cex = 1.5)
    # add row labels to left margin of plot
    mtext(rev(1:n_row), side = 2, at = 1:n_row, line = 0.5, cex = 1.5)

    # draw a box around the plot
    box()

    ### Fill plot with symbols and numbers
    for(i in 1:n_row) {
        for(j in 1:n_col) {
            # only print symbol if card is face UP and available
            if ((board[i, j, 3] == 1) & (board[i, j, 4] == 0)) {
                pch_val <- board[i, j, 1]
                col_val <- board[i, j, 2]
                points(j, (n_row-i)+1, pch = pch_val, cex=3, col=col_val)

            }
            # print a card that is already won by a player
            if (board[i, j, 4] > 0) {
                text(j, (n_row-i)+1, board[i, j, 4], pos = 4, cex = 1.5)
            }
        }
    }
}

print_leaderboard <- function(leaderboard, p_names, player, computer, user_inputs) {
    cat("Correct, Player", player, "plays again! Current Leaderboard:\n")
    # Leaderboard

    # first line of leaderboard with names
    cat(paste(sprintf("%10s", p_names), sep = ""), "\n")
    # second line of output with scores
    cat(paste(sprintf("%10d", leaderboard), sep = ""), "\n")

    # Continue with game
    getUserInput(card = FALSE, computer = computer, first_card = NULL, user_inputs = user_inputs)
    assign("user_inputs", user_inputs, envir = parent.frame())  # Update global user_inputs variable
}

print_winners <- function(leaderboard) {
    # find all players with maximum score
    max_val <- max(leaderboard)
    top_players <- which(leaderboard == max_val)

    # single winner
    if (length(top_players) == 1) {
        cat("Correct, Player", top_players[1], "wins! Final leaderboard:\n")
    } else {
    # multiple winners
        cat("Correct, Player", top_players[1])
        for (i in 2:length(top_players)) {
            cat(" and Player", top_players[i])
        }
        cat(" are tied winners! Final leaderboard:\n")
    }
}