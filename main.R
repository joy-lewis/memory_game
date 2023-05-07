# main game class

print_board <- function(board, n_row, n_col, free) {
    # board entrys follow this pattern:
    #     pch, symbols from 1 to 13
    #     col, colors from 1 to 8
    plot(0, 0, main = "", xlab = "", ylab = "",
        xlim = c(1, n_col), ylim = c(n_row, 1), xaxt = "n", axes = FALSE)
    title(main = "Memory", font.main = 2, line = 3)
    # remove the tick marks on y-axis and x-axis
    axis(2, at = n_row:1, lwd.ticks = 0, cex.axis = 1.5)
    axis(3, at = 1:n_col, lwd.ticks = 0, cex.axis = 1.5)
    box()
    # extract values for symbol and color from current board
    for (i in 1:n_row){
        for (j in 1:n_col){
            # only print symbol if card is face UP
            if ((board[i, j, 3] == 1) && (board[i, j, 4] == 0)) {
                pch_val <- board[i, j, 1]
                col_val <- board[i, j, 2]
                points(i, j, pch = pch_val, col = col_val)
            }
        }
    }
}

get_free_cards <- function(board, n_row, n_col){
    free <- list() # free card coordiantes

    for (i in 1:n_row) {
        for (j in 1:n_col) {
            if (board[i, j, 4] == 0) { # check if card is available
                free <- c(free, list(c(i, j)))  # add the pair to the list
            }
        }
    }
    return(free)
}

evalaute_move <- function(board, coord1, coord2){
    result <- ((board[coord1[1], coord1[2], 1] == board[coord2[1], coord2[2], 1]) 
             && (board[coord1[1], coord1[2], 2] == board[coord2[1], coord2[2], 2]))
    return(result)
}

move_real <- function(board, player, n_row, n_col) {
    # get position of 1st card which should be turned face up
    cat("Player", player, ", choose your first card (1: row, 2: column)!\n")
    input1 <- scan(n = 2, what = integer(), quiet = TRUE)
    board[input1[1], input1[2], 3] <- 1 # signals that this card is face UP
    print_board(board, n_row, n_col)

    # get position of 2nd card which should be turned face up
    cat("Player", player, ", choose your second card (1: row, 2: column)!\n")
    input2 <- scan(n = 2, what = integer(), quiet = TRUE)
    board[input2[1], input2[2], 3] <- 1
    print_board(board, n_row, n_col)
    
    # check if both cards have same symbol and same color
    return(evalaute_move(board, input1, input2))
}

move_computer <- function(board, player, n_row, n_col) {
    # get position of 1st card which should be turned face up
    free <- get_free_cards(board, n_row, n_col)
    card1 <- sample(free, 1)
    free_without_card1 <- setdiff(free, card1)
    card2 <- sample(free_without_card1, 1)
    cat("Player", player, ", chooses cards", card1, "and", card2, "\n")

    # print new board
    board[card1[1], card1[2], 3] <- 1
    board[card2[1], card2[2], 3] <- 1
    print_board(board, n_row, n_col)

    # check if both cards have same symbol and same color
    return(evalaute_move(board, card1, card2))
}

init_board <- function(n_row, n_col){
    # empty playing board, with dimensions:
    # [row position, column position, symbol
    # & color & face up (1) or down (0) & not available]
    board <- array(0, dim = c(n_row, n_col, 4))

    ## Plotting initial game state
    print_board(board, n_row, n_col)
    return(board)
}

check_game_ended <- function(board, n_row, n_col) {
    # returns true if all values of 3rd dimension and 4th value are 1,
    # meaning non of the cards are available anymore

    # CHECK FOR EDGE CASES
}

memory <- function(n_row = 4, n_col = 4, n_real = 2, n_computer = 0) {
    n <- n_row * n_col # number of cards

    ## Error handling
    if (n > 208) {
        stop("Error in memory(): Too many cards: 
            n_row * n_col must not exceed 208.\n")
        geterrmessage()
    }

    ## Get initial player
    set.seed(0) # JUST FOR TESTING !!!!!!
    all_players <- seq(from = 1, to = n_real + n_computer)
    player <- sample(all_players, 1) # current player
    cat("Player", player, "starts!\n")
    cat("In each move you have to choose two cards.\n\n")

    ## Build initial board
    board <- init_board(n_row, n_col)

    ## Players take turns until game has ende
    game_ended <- FALSE

    while (!game_ended) {
        if (player <= n_real) { # move for real player
            success <- move_real(board, player, n_row, n_col)
            if (success) {
                # 1) set 4th value of 3rd dimension for both cards to 1,
                # since they are not available anymore
                # 2) update leaderboard
            } else {
                # set 3rd value of 3rd dimension back to 0 since the cards
                # didnt match and therefore need to be faced down again
            }
        } else { # move for computer player
            success <- move_computer(board, player, n_row, n_col)
            if (success) {
                # 1) set 4th value of 4th dimension for both cards to 1,
                # since they are not available anymore
                # 2) update leaderboard
            } else {
                # set 3rd value of 3rd dimension back to 0 since the cards
                # didnt match and therefore need to be faced down again
            }
        }
        # check if game has ended
        game_ended <- check_game_ended(board, n_row, n_col)
        # get the next player (e.g. current player is 2 then next player is 3)
        player <- (player + 1) %% (n_real + n_computer)
    }
}