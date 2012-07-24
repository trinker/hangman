\name{hangman}
\alias{hangman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hangman()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    if (!"qdap" \%in\% .packages(all.available = TRUE)) {
        if (!"qdap" \%in\% .packages(all.available = TRUE)) {
            stop("please install devtools package")
        }
        require(devtools)
        install_github("qdap", "trinker")
    }
    require(qdap)
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
    hang.plot <- function() {
        plot.new()
        mtext("HANGMAN", col = "blue", cex = 2)
        mtext(paste(x2, collapse = " "), side = 1, cex = 1.5)
        mtext(paste(rep("_", len), collapse = " "), side = 1, 
            cex = 1.5)
        mtext("wrong", side = 3, cex = 1.5, adj = 0, padj = 1, 
            col = "red")
        mtext(paste(wrong, collapse = "\n"), side = 3, cex = 1.5, 
            adj = 0, padj = 2.5)
        mtext(paste(right, collapse = "\n"), side = 3, cex = 1.5, 
            adj = 1, padj = 2.5)
        mtext("correct", side = 3, cex = 1.5, adj = 1, padj = 1, 
            col = "red")
        segments(0.365, 0.77, 0.365, 0.83, lwd = 2)
        segments(0.365, 0.83, 0.625, 0.83, lwd = 2)
        segments(0.625, 0.83, 0.625, 0.25, lwd = 2)
        segments(0.57, 0.25, 0.675, 0.25, lwd = 2)
        parts <- seq_along(length(wrong))
        if (identical(wrong, character(0))) {
            parts <- 0
        }
        if (1 \%in\% parts) {
            mtext("O", side = 1, cex = 4, adj = 0.365, padj = -7.2)
            mtext("o o", side = 1, cex = 1, adj = 0.3725, padj = -28.2)
            mtext("<", side = 1, cex = 1, adj = 0.373, padj = -27.6)
            mtext("__", side = 1, cex = 1, adj = 0.373, padj = -27.2)
        }
        if (2 \%in\% parts) {
            mtext("I", side = 1, cex = 4, adj = 0.375, padj = -6.25)
            mtext("I", side = 1, cex = 4, adj = 0.375, padj = -5.5)
            mtext("I", side = 1, cex = 4, adj = 0.375, padj = -4.75)
        }
        if (3 \%in\% parts) {
            segments(0.37, 0.57, 0.45, 0.63, lwd = 7)
        }
        if (4 \%in\% parts) {
            segments(0.37, 0.57, 0.29, 0.63, lwd = 7)
        }
        if (5 \%in\% parts) {
            segments(0.37, 0.426, 0.43, 0.3, lwd = 7)
            mtext("__", side = 1, cex = 1, adj = 0.373, padj = -27.2, 
                col = "white")
            mtext("O", side = 1, cex = 1.25, adj = 0.373, padj = -21.5, 
                col = "red")
        }
        if (6 \%in\% parts) {
            segments(0.37, 0.426, 0.31, 0.3, lwd = 7)
            mtext("o o", side = 1, cex = 1, adj = 0.3725, padj = -28.2, 
                col = "white")
            mtext("x x", side = 1, cex = 1, adj = 0.3725, padj = -28.2)
            mtext("You Lose", side = 1, cex = 8, padj = -3, col = "darkgreen")
        }
        if (win1 == len) {
            mtext("WINNER!", side = 1, cex = 8, padj = -3, col = "green")
            mtext("WINNER!", side = 1, cex = 8, adj = 0.1, padj = -3.1, 
                col = "darkgreen")
        }
    }
    guess <- function() {
        cat("\n", "Choose a letter:", "\n")
        y <- scan(n = 1, what = character(0), quiet = T)
        if (y \%in\% c(right, wrong)) {
            stop(paste0("You've already guessed ", y))
        }
        if (y \%in\% x) {
            right <<- c(right, y)
            win1 <<- sum(win1, sum(x \%in\% y))
            win <<- win1/len
            message(paste0("Correct!", "\n"))
        }
        else {
            wrong <<- c(wrong, y)
            chance <<- length(wrong)
            message(paste0("The word does not contain ", y, "\n"))
        }
        x2[x \%in\% right] <<- x[x \%in\% right]
        print(x2, quote = FALSE)
        hang.plot()
    }
    hang.plot()
    while (all(win1 != len & chance < 6)) {
        try(guess())
    }
    if (win == 1) {
        outcome <- "Congratulations! You Win!\n"
    }
    else {
        outcome <- paste("Sorry. You loose. The word is:", x1, 
            "\n")
    }
    print(outcome)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
