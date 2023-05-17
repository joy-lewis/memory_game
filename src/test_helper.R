# Functions that are required for running tests
getUserInput <- function(board=NULL, n_row=0, n_col=0, card, computer, first_card = NULL, user_inputs) {
    # this seperates the user input from the actual game,
    # in order to test the game later
    test <- (!is.null(user_inputs)) # True if we conduct a test

    if (card) {
        # Read a card
        valid <- FALSE
        input <- c(0, 0)
        while (!valid) {
            # get input from user or predefined input sequence (when testing)
            if (test) {
                input <- as.integer(c(user_inputs[1], user_inputs[2]))
                user_inputs <- user_inputs[-c(1, 2)]  # Remove the used inputs
                assign("user_inputs", user_inputs, envir = parent.frame())  # Update global user_inputs variable
            } else {
                input <- scan(n = 2, what = integer(), quiet = TRUE)
            }
            # check if card is valid
            if (valid_input(board, input, n_row, n_col) && (!identical(first_card, input))){
                valid <- TRUE
            } else {
                cat("Card not valid. Again: \n")
            }
        }
        return(input)
    } else {
        # Continue with game
        valid <- FALSE
        key <- ""
        while (!valid) {
            cat("Press [y], when you are ready to move on!\n")
            # Proceed automatically if computer plays
            if (!computer) { 
                # get input from user or predefined input sequence (when testing)
                if (test) {
                    key <- as.character(user_inputs[1])
                    user_inputs <- user_inputs[-1] # Remove the used input
                    assign("user_inputs", user_inputs, envir = parent.frame())  # Update global user_inputs variable
                } else {
                    key <- scan(what = character(), n = 1, quiet = TRUE)
                }
            } else { key <- "y" }
            # Check if user pressed correct key
            if (key == "y") {
                valid <- TRUE
            } else {
                cat("Wrong key!")
            }
        }
    }
    #assign("user_inputs", user_inputs, envir = parent.frame())  # Update global user_inputs variable
}