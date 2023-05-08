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
            # only print symbol if card is face UP and available
            if ((board[i, j, 3] == 1) && (board[i, j, 4] == 0)) {
                pch_val <- board[i, j, 1]
                col_val <- board[i, j, 2]
                points(i, j, pch = pch_val, col = col_val)
            }
            # print a card that is already won by a player
            if (board[i, j, 4] > 0) {
                text(i, j, board[i, j, 5])
            }
        }
    }
}

get_free_cards <- function(board, n_row, n_col){
    free <- list() # free card coordiantes

    for (i in 1:n_row) {
        for (j in 1:n_col) {
            if (board[i, j, 5] == 0) { # check if card was already won by a player
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

valid_input <- function(board, input, n_row, n_col) {
    b1 <- (input[1] %in% 1:n_row) # row within range
    b2 <- (input[2] %in% 1:n_col) # column within range
    b3 <- (board[input[1], input[2], 3] == 0) # card is face DOWN
    b4 <- (board[input[1], input[2], 4] == 0) # card wasn't won by anyone yet
    return(all(b1, b2, b3, b4))
}

move_real <- function(board, player, n_row, n_col) {
    # get position of 1st card which should be turned face up
    cat("Player", player, ", choose your first card (1: row, 2: column)!\n")

    valid <- FALSE
    input <- c(0, 0)
    while (!valid) {
        input1 <- scan(n = 2, what = integer(), quiet = TRUE)
        if (valid_input(input)){
            valid <- TRUE
        } else {
            cat("Card not valid. Again: \n")
        }
    }
    board[input1[1], input1[2], 3] <- 1 # signals that this card is face UP
    print_board(board, n_row, n_col)

    # get position of 2nd card which should be turned face up
    cat("Player", player, ", choose your second card (1: row, 2: column)!\n")

    valid <- FALSE
    input2 <- c(0, 0)
    while (!valid) {
        input2 <- scan(n = 2, what = integer(), quiet = TRUE)
        if (valid_input(input)){
            valid <- TRUE
        } else {
            cat("Card not valid. Again: \n")
        }
    }
    board[input2[1], input2[2], 3] <- 1
    print_board(board, n_row, n_col)
    
    # check if both cards have same symbol and same color
    return(c(evalaute_move(board, input1, input2), input1, input2))
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
    return(c(evalaute_move(board, card1, card2), card1, card2))
}

init_board <- function(n, n_row, n_col) {
    # board position is encoded in 1st and 2nd dimension
    # board 3rd dimension meaning:
    # 1. symbol
    # 2. color
    # 3. face up (1) or down (0)
    # 4. some value x > 0, means this card was won by player x
    board <- array(0, dim = c(n_row, n_col, 4))

    # Generate random cards within given pch and col ranges using sample()
    # pch values
    board[, , 1] <- matrix(sample(1:13, n, replace = TRUE), nrow = n_row)
    # color values
    board[, , 2] <- matrix(sample(1:8, n, replace = TRUE), nrow = n_row)

    # Plotting initial game state
    print_board(board, n_row, n_col)
    return(board)
}

init_player <- function(n_real, n_computer){
    set.seed(0) # JUST FOR TESTING !!!!!!

    all_players <- seq(from = 1, to = n_real + n_computer)
    player <- sample(all_players, 1)
    cat("Player", player, "starts!\n")
    cat("In each move you have to choose two cards.\n\n")
    return(player) # current player
}

print_leaderboard <- function(leaderboard, player) {
    cat("Correct, Player", player, "plays again! Current Leaderboard:\n")
    # Leaderboard
    for (i in 1:length(leaderboard)){
        cat(sprintf("%-10s %-10s\n", i, leaderboard[i]))
    }

    # Continue with game
    valid <- FALSE
    key <- ""
    while (!valid) {
        cat("Press [y], when you are ready to move on!\n")
        key <- scan(what = character(), n = 1, quiet = TRUE)
        if (key == "y") {
            valid <- TRUE
        } else {
            cat("Wrong, Player", player, "plays!")
        }
    }
}


equal_cards <- function(board, card1, card2, player){
    # 1) set 4th value of 3rd dimension for both cards to 1,
    # since they are not available anymore
    # 2) set 5th value of 3rd dimension for both cards to the player value
    # since the current player won tboth cards
    # 3) the player stays the same

    board[card1[1], card1[2], 4] <- 1
    board[card2[1], card2[2], 4] <- 1
    board[card1[1], card1[2], 5] <- player
    board[card2[1], card2[2], 5] <- player
    return(board)
}

unequal_cards <- function(board, card1, card2, player, n_real, n_computer) {
    # 1) set 3rd value of 3rd dimension back to 0 since the cards
    # didnt match and therefore need to be faced down again
    # 2) get the next player (e.g. current player is 2 then next player is 3

    player <- (player + 1) %% (n_real + n_computer)
    board[card1[1], card1[2], 3] <- 0
    board[card2[1], card2[2], 3] <- 0
    return(c(board, player))
}

check_game_ended <- function(board, n_row, n_col) {
    # count number of ones in 2d vector board[ , , 3], meaning number of available vards

    # -> if number == 1 than print board with remainign card face up, winner/s, leaderboard & return True

    # -> if number == 0 than print board as it is, winner/s, leaderboard & return True
    
    # -> else Return False
}

memory <- function(n_row = 4, n_col = 4, n_real = 2, n_computer = 0) {
    n <- n_row * n_col # total number of cards
    leaderboard <- rep(0, n) # leaderboard for all players

    ## Error handling
    if (n > 208) {
        stop("Error in memory(): Too many cards: 
            n_row * n_col must not exceed 208.\n")
        geterrmessage()
    }

    ## Get initial player
    player <- init_player(n_real, n_computer)

    ## Build initial board
    board <- init_board(n, n_row, n_col)

    ## Players take turns until game has ende
    game_ended <- FALSE

    while (!game_ended) {
        if (player <= n_real) { # move for real player
            success <- move_real(board, player, n_row, n_col)
            if (success[1]) {
                equal <- equal_cards(board, success[2], success[3], player, n_real, n_computer)
                board <- equal
                leaderboard[player] <- leaderboard[player] + 1
                print_leaderboard(leaderboard, player)
            } else {
                unequal <- unequal_cards(board, success[2], success[3], player, n_real, n_computer)
                board <- unequal[1]
                player <- unequal[2]
            }
        } else { # move for computer player
            success <- move_computer(board, player, n_row, n_col)
            if (success[1]) {
                equal <- equal_cards(board, success[2], success[3], player, n_real, n_computer)
                board <- equal
                update_leaderboard(player)
            } else {
                unequal <- unequal_cards(board, success[2], success[3], player, n_real, n_computer)
                board <- unequal[1]
                player <- unequal[2]
            }
        }
        # check if game has ended
        game_ended <- check_game_ended(board, n_row, n_col)
    }
}