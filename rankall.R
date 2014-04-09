rankall <- function(outcome, num = "best") {

    sel <- function(sorted) {
        if (num == "best") {
            sorted[1, 1]
        } else if (num == "worst") {
            sorted[nrow(sorted), 1]
        } else if (num <= nrow(sorted)) {
            sorted[num, 1]
        } else {
            NA
        }
    }

    col <- if (outcome == "heart attack") {
        11
    } else if (outcome == "heart failure") {
        17
    } else if (outcome == "pneumonia") {
        23
    } else {
        stop("invalid outcome")
    }

	cc <- c(rep("NULL", 46))
	cc[c(2, 7, col)] <- "character"
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = cc)
    names(outcome) <- c("hospital", "state", "mortality")
    outcome$mortality <- suppressWarnings(as.numeric(outcome$mortality))
    outcome <- na.omit(outcome)
    sortedOutcome <- outcome[order(outcome$state, outcome$mortality, outcome$hospital), ]
	state <- unique(sortedOutcome$state)
	hospital <- sapply(split(sortedOutcome, sortedOutcome$state), sel)
	data.frame(hospital, state)
}
