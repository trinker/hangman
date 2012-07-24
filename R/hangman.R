hangman <-
function() {
    x1 <- DICTIONARY[sample(1:nrow(DICTIONARY), 1), 1]
    x <- unlist(strsplit(x1, NULL))
    len <- length(x)
    x2 <- rep("_", len)
    chance <- 0
    win1 <- 0
    win <- win1/len
    wrong <- character()
    right <- character()
    print(x2, quote = FALSE)
    hang.plot <- function(){ #plotting function
        plot.new()
        mtext("HANGMAN", col = "blue", cex=2)    
        mtext(paste(x2, collapse = " "), side = 1, cex=1.5) 
        mtext("wrong", side = 3, cex=1.5,
            adj = 0, padj = 1, col = "red") 
        mtext(paste(wrong, collapse = "\n"), side = 3, cex=1.5,
            adj = 0, padj = 2.5)
        mtext(paste(right, collapse = "\n"), side = 3, cex=1.5,
            adj = 1, padj = 2.5) 
        mtext("correct", side = 3, cex=1.5,
            adj = 1, padj = 1, col = "red")
        segments(.365, .77, .365, .83, lwd=2)
        segments(.365, .83, .625, .83, lwd=2)
        segments(.625, .83, .625, .25, lwd=2)
        segments(.57, .25, .675, .25, lwd=2)
        parts <- seq_len(length(wrong))
        if (identical(wrong, character(0))) {
            parts <- 0
        }
        if (1 %in% parts) {
            mtext("O", side = 1, cex=4, adj = .365, padj = -7.2)
            mtext("o o", side = 1, cex=1, adj = .3725, padj = -28.2)
            mtext("<", side = 1, cex=1, adj = .373, padj = -27.6)
            mtext("__", side = 1, cex=1, adj = .373, padj = -27.2)
        }
        if (2 %in% parts) {
            mtext("I", side = 1, cex=4, adj = .375, padj = -6.25)
            mtext("I", side = 1, cex=4, adj = .375, padj = -5.5)
            mtext("I", side = 1, cex=4, adj = .375, padj = -4.75)
        }
        if (3 %in% parts) {
            segments(.37, .57, .45, .63, lwd=7)
        }
        if (4 %in% parts) {
            segments(.37, .57, .29, .63, lwd=7)
        }
        if (5 %in% parts) {
            segments(.37, .426, .43, .3, lwd=7)
            mtext("__", side = 1, cex = 1, adj = .373, 
                padj = -27.2, col = "white")
            mtext("O", side = 1, cex = 1.25, adj = .373, padj = -21.5, 
                col="red")
        }
        if (6 %in% parts) {
        	segments(.37, .426, .31, .3, lwd = 7)
        	mtext("o o", side = 1, cex = 1, adj = .3725, 
        		  padj = -28.2, col="white")
        	mtext("x x", side = 1, cex=1, adj = .3725, padj = -28.2)
        	mtext("You Lose", side = 1, cex=8, padj = -3, 
        		  col = "darkgreen")
        	mtext(paste(x2, collapse = " "), side = 1, cex=1.6, col="white") 
        	mtext(paste(x2, collapse = " "), side = 1, cex=1.5, col="white") 
        	mtext(paste(x2, collapse = " "), side = 1, adj = .51, cex=1.6, col="white")
        	mtext(paste(x, collapse = " "), side = 1, cex=1.5)
        }
        if (win1 == len) {
            mtext("WINNER!", side = 1, cex=8, padj = -3, 
                col = "green")
            mtext("WINNER!", side = 1, cex=8, adj = .1, padj = -3.1, 
                col = "darkgreen")
        }
    } #end of hang.plot
    guess <- function(){#start of guess function
        cat("\n","Choose a letter:","\n") 
        y <- scan(n=1,what = character(0),quiet=T)
        if (y %in% c(right, wrong)) {
            stop(paste0("You've already guessed ", y))
        }
        if (!y %in% letters) {
        	stop(paste0(y, " is not a letter"))
        }
        if (y %in% x) {
            right <<- c(right, y)
            win1 <<- sum(win1, sum(x %in% y)) 
            win <<- win1/len 
            message(paste0("Correct!","\n"))
        } else {
            wrong  <<- c(wrong, y)
            chance  <<- length(wrong)
            message(paste0("The word does not contain ", y, "\n"))
        }
        x2[x %in% right] <<- x[x %in% right]
        print(x2, quote = FALSE)
        hang.plot()
    }#end of guess function
    hang.plot()
    while(all(win1 != len & chance < 6)){ 
        try(guess())
    } 
    if (win == 1) {
        outcome <- "\nCongratulations! You Win!\n"
    } else {
        outcome <- paste("\nSorry. You loose. The word is:", x1, "\n")
    }
    cat(outcome)
}
