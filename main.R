# main game class
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

get_free_cards <- function(board, n_row, n_col){
    free <- list() # free card coordiantes

    for (i in 1:n_row) {
        for (j in 1:n_col) {
            if (board[i, j, 4] == 0) { # check if card was already won by a player
                free <- c(free, list(c(i, j)))  # add the pair to the list
            }
        }
    }
    return(free)
}

evalaute_move <- function(board, coord1, coord2){
    result <- ((board[coord1[1], coord1[2], 1] == board[coord2[1], coord2[2], 1]) 
              & (board[coord1[1], coord1[2], 2] == board[coord2[1], coord2[2], 2]))
    return(result)
}

valid_input <- function(board, input, n_row, n_col) {
    b1 <- (input[1] >= 1) & (input[1] <= n_row) # row within range
    b2 <- (input[2] >= 1) & (input[2] <= n_col) # column within range
    if (!b1 | !b2) { return(FALSE) }
    
    b3 <- (board[input[1], input[2], 3] == 0) # card is face DOWN
    b4 <- (board[input[1], input[2], 4] == 0) # card wasn't won by anyone yet
    
    if (!b3 | !b4) { return(FALSE) }
    return(TRUE)
}

move_real <- function(board, player, n_row, n_col) {
    # get position of 1st card which should be turned face up
    cat("Player", player, ", choose your first card (1: row, 2: column)!\n")

    valid <- FALSE
    input1 <- c(0, 0)
    while (!valid) {
        input1 <- scan(n = 2, what = integer(), quiet = TRUE)
        if (valid_input(board, input1, n_row, n_col)){
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
        if (!identical(input1, input2) & valid_input(board, input2, n_row, n_col)){
            valid <- TRUE
        } else {
            cat("Card not valid. Again: \n")
        }
    }
    board[input2[1], input2[2], 3] <- 1
    print_board(board, n_row, n_col)
    
    # check if both cards have same symbol and same color
    return(list(evalaute_move(board, input1, input2), input1, input2))
}

move_computer <- function(board, player, n_row, n_col) {
    # find two free cards which should be turned face up
    free <- get_free_cards(board, n_row, n_col)
    card1 <- sample(free, 1)
    free_without_card1 <- setdiff(free, card1)
    card2 <- sample(free_without_card1, 1)

    card1 <- unlist(card1) # to print the values bellow
    card2 <- unlist(card2)

    cat("Player", player, ", chooses cards (", card1[1], ",", card1[2],") and (", card2[1], ",", card2[2], ") \n")

    # print new board
    board[card1[1], card1[2], 3] <- 1
    board[card2[1], card2[2], 3] <- 1
    print_board(board, n_row, n_col)

    # check if both cards have same symbol and same color
    return(list(evalaute_move(board, card1, card2), card1, card2))
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
    pairs <- as.matrix(expand.grid(x = 1:13, y = 1:8)) # all possible combinations of pch and col

    # we need a list to sample n cards
    pairs_list <- split(pairs, 1:nrow(pairs))
    pairs_list <- as.list(pairs_list)

    # get playing cards, including "left over card" if n is uneven
    n_rest <- (n %% 2)
    n_new <- (n - n_rest) / 2

    playing_cards <- sample(pairs_list, n_new, replace = FALSE)
    playing_cards <- c(playing_cards, playing_cards) # every card must appear twice

    if (n_rest == 1) { # add "left over card" to the list
        available_pairs <- setdiff(pairs_list, playing_cards)
        left_over_card <- sample(available_pairs, 1, replace = FALSE)
        playing_cards <- c(playing_cards, left_over_card)
    }
    playing_cards <- sample(playing_cards) # shuffle the cards ("left over card" at random position)

    # assign pch color values to the board
    card_i <- 1
    for (i in 1:n_row) {
        for (j in 1:n_col) {
            board[i, j, 1] <- playing_cards[[card_i]][1]
            board[i, j, 2] <- playing_cards[[card_i]][2]

            card_i <- (card_i + 1)
        }
    }

    # Plotting initial game state
    print_board(board, n_row, n_col)
    return(board)
}

init_player <- function(n_real, n_computer){
    all_players <- seq(from = 1, to = n_real + n_computer)
    player <- sample(all_players, 1)
    cat("Player", player, "starts!\n")
    cat("In each move you have to choose two cards.\n\n")
    return(player) # current player
}

print_leaderboard <- function(leaderboard, p_names, player, computer) {
    cat("Correct, Player", player, "plays again! Current Leaderboard:\n")
    # Leaderboard

    # first line of leaderboard with names
    cat(paste(sprintf("%10s", p_names), sep = ""), "\n")
    # second line of output with scores
    cat(paste(sprintf("%10d", leaderboard), sep = ""), "\n")

    # Continue with game
    valid <- FALSE
    key <- ""
    while (!valid) {
        cat("Press [y], when you are ready to move on!\n")
        # Proceed automatically if computer plays
        if (!computer) {
            key <- scan(what = character(), n = 1, quiet = TRUE)
        } else { key <- "y" }
        # Check if user pressed correct key
        if (key == "y") {
            valid <- TRUE
        } else {
            cat("Wrong key!")
        }
    }
}

equal_cards <- function(board, card1, card2, player){
    # 1) set 4th value of 3rd dimension for both cards to 1,
    # since the current player won both cards
    # 2) the player stays the same

    board <- board
    board[card1[1], card1[2], 4] <- player
    board[card2[1], card2[2], 4] <- player
    return(board)
}

unequal_cards <- function(board, card1, card2, player, num_players, computer) {
    # 1) set 3rd value of 3rd dimension back to 0 since the cards
    # didnt match and therefore need to be faced down again
    # 2) get the next player (e.g. current player is 2 then next player is 3

    next_player <- (player %% num_players)+1 # chosse next player
    cat("Wrong, Player", next_player, "plays! ")

    # Continue with game
    valid <- FALSE
    key <- ""
    while (!valid) {
        cat("Press [y], when you are ready to move on!\n")
        # Proceed automatically if computer plays
        if (!computer) { 
            key <- scan(what = character(), n = 1, quiet = TRUE)
        } else { key <- "y" }
        # Check if user pressed correct key
        if (key == "y") {
            valid <- TRUE
        } else {
            cat("Wrong key!")
        }
    }

    board <- board
    board[card1[1], card1[2], 3] <- 0
    board[card2[1], card2[2], 3] <- 0
    return(board)
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

check_game_ended <- function(board, n_row, n_col, leaderboard, player, p_names, computer) {
    # count number of ones in 2d slice board[ , , 3], i.e. count available cards
    count <- sum(board[, , 4] == 0)
    
    # -> if number == 1 than print board with remaining card face up, winner/s, leaderboard & return True
    if (count == 1) {
        board[, , 3] <- 1 # set all cards face UP
    }
    # -> if number == 0 than print board as it is, winner/s, leaderboard & return True
    if (count <= 1) {
        print_board(board, n_row, n_col)
        print_winners(leaderboard)
        
        # Leaderboard
        # first line of leaderboard with names
        cat(paste(sprintf("%10s", p_names), sep = ""), "\n")
        # second line of output with scores
        cat(paste(sprintf("%10d", leaderboard), sep = ""), "\n")

        return(TRUE)
    } else {
        # print regular leaderboard since game is still going on
        print_leaderboard(leaderboard, p_names, player, computer)
        return(FALSE)
    }
}

memory <- function(n_row = 4, n_col = 4, n_real = 2, n_computer = 0) {
    #set.seed(0) # JUST FOR TESTING !!!!!!

    n <- n_row * n_col # total number of cards
    num_players <- n_real + n_computer
    leaderboard <- rep(0, n_real + n_computer) # leaderboard for all players

    ## Error handling
    if (n > 208) {
        stop("Error in memory(): Too many cards: 
            n_row * n_col must not exceed 208.\n")
        geterrmessage()
    }

    ## Get initial player
    player <- init_player(n_real, n_computer)

    # collect player names
    p_names <- c()
    for (i in 1:length(leaderboard)){
        p_names <- c(p_names, paste("Player", i))
    }

    ## Build initial board
    board <- init_board(n, n_row, n_col)

    ## Players take turns until game has ende
    game_ended <- FALSE

    while (!game_ended) {
        if (player <= n_real) { # move for real player
            success <- move_real(board, player, n_row, n_col)
            if (success[1]==1) {
                equal <- equal_cards(board, unlist(success[2]), unlist(success[3]), player)
                board <- equal
                leaderboard[player] <- leaderboard[player] + 1
                # check if game has ended
                game_ended <- check_game_ended(board, n_row, n_col, leaderboard, player, p_names, FALSE)
            } else {
                unequal <- unequal_cards(board, unlist(success[2]), unlist(success[3]), player, num_players, FALSE)
                board <- unequal
                player <- (player %% num_players)+1 # chosse next player
            }
        } else { # move for computer player
            success <- move_computer(board, player, n_row, n_col)
            if (success[1]==1) {
                equal <- equal_cards(board, unlist(success[2]), unlist(success[3]), player)
                board <- equal
                leaderboard[player] <- leaderboard[player] + 1
                # check if game has ended
                game_ended <- check_game_ended(board, n_row, n_col, leaderboard, player, p_names, TRUE)
            } else {
                unequal <- unequal_cards(board, unlist(success[2]), unlist(success[3]), player, num_players, TRUE)
                board <- unequal
                player <- (player %% num_players)+1 # chosse next player
            }
        }
    }
}