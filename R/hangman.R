hangman <- 
function(reset.score = FALSE) {
    opar <- par()$mar
    on.exit(par(mar = opar))
    par(mar = rep(0, 4))
    x1 <- DICTIONARY[sample(1:nrow(DICTIONARY), 1), 1]
    x <- unlist(strsplit(x1, NULL))
    len <- length(x)
    x2 <- rep("_", len)
    chance <- 0
    if(!exists("wins", mode="numeric", envir = .GlobalEnv)  | reset.score){
        assign("wins", 0, envir = .GlobalEnv)
    }
    if(!exists("losses", mode="numeric", envir = .GlobalEnv) | reset.score){
        assign("losses", 0, envir = .GlobalEnv)
    }
    win1 <- 0
    win <- win1/len
    wrong <- character()
    right <- character()
    print(x2, quote = FALSE)
    circle <- function(x, y, radius, units=c("cm", "in"), segments=100,  
        lwd = NULL){ 
        units <- match.arg(units) 
        if (units == "cm") radius <- radius/2.54 
        plot.size <- par("pin") 
        plot.units <- par("usr") 
        units.x <- plot.units[2] - plot.units[1] 
        units.y <- plot.units[4] - plot.units[3] 
        ratio <- (units.x/plot.size[1])/(units.y/plot.size[2]) 
        size <- radius*units.x/plot.size[1] 
        angles <- (0:segments)*2*pi/segments 
        unit.circle <- cbind(cos(angles), sin(angles)) 
        shape <- matrix(c(1, 0, 0, 1/(ratio^2)), 2, 2) 
        ellipse <- t(c(x, y) + size*t(unit.circle %*% chol(shape))) 
        lines(ellipse, lwd = lwd) 
    } #taken from John Fox: http://tolstoy.newcastle.edu.au/R/help/06/04/25821.html
    hang.plot <- function(){ #plotting function
        plot.new()
        parts <- seq_len(length(wrong))
        if (identical(wrong, character(0))) {
            parts <- 0
        }
        text(.5, .9, "HANGMAN", col = "blue", cex=2)   
        if (!6 %in% parts) { 
            text(.5, .1, paste(x2, collapse = " "), cex=1.5) 
        }
        text(.05, .86, "wrong", cex=1.5, col = "red") 
        text(.94, .86,"correct", cex=1.5, col = "red")
        text(.05, .83, paste(wrong, collapse = "\n"), offset=.3, cex=1.5, 
            adj=c(0,1))
        text(.94, .83, paste(right, collapse = "\n"), offset=.3, cex=1.5, 
            adj=c(0,1))
        segments(.365, .77, .365, .83, lwd=2)
        segments(.365, .83, .625, .83, lwd=2)
        segments(.625, .83, .625, .25, lwd=2)
        segments(.57, .25, .675, .25, lwd=2)
        if (1 %in% parts) {
            circle(.365, .73, .7, lwd=4)
            if (!6 %in% parts) { 
                text(.365, .745, "o o", cex=1)
            }
            if (!5 %in% parts) { 
                text(.365, .71, "__", cex = 1)
            }
        text(.36, .73, "<", cex=1)
        }
        if (2 %in% parts) {
            segments(.365, .685, .365, .4245, lwd=7)
        }
        if (3 %in% parts) {
            segments(.365, .57, .45, .63, lwd=7)
        }
        if (4 %in% parts) {
            segments(.365, .57, .29, .63, lwd=7)
        }
        if (5 %in% parts) {
            segments(.365, .426, .43, .3, lwd=7)
            text(.365, .71, "O", cex = 1.25, col = "red")
        }
        if (6 %in% parts) {
            segments(.365, .426, .31, .3, lwd = 7)
            text(.365, .745, "x  x", cex=1)
            text(.5, .5, "You Lose", cex=8, col = "darkgreen") 
            text(.5, .1, paste(x, collapse = " "), cex=1.5) 
        }
        if (win1 == len) {
            text(.5, .5, "WINNER!", cex=8, col = "green")
            text(.505, .505, "WINNER!", cex=8, col = "darkgreen")
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
        assign("wins", wins + 1, envir = .GlobalEnv)
    } else {
        outcome <- paste("\nSorry. You lose. The word is:", x1, "\n")
        assign("losses", losses + 1, envir = .GlobalEnv)
    }
        cat(outcome)
        cat(paste0("\nwins: ", wins, " | losses: ", losses, "\n"))
        text(.5, .2, paste0("wins: ", wins, "  |  losses: ", 
            losses), cex = 3, col = "violetred")
}