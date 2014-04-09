best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    col <- if (outcome == "heart attack") {
        11
    } else if (outcome == "heart failure") {
        17
    } else if (outcome == "pneumonia") {
        23
    } else {
        stop("invalid outcome")
    }

    data.state <- subset(data, State == state, select = c(col, 2))
    if (nrow(data.state) == 0) {
        stop("invalid state")
    }

    data.state[, 1] <- suppressWarnings(as.numeric(data.state[, 1]))
    sorted <- data.state[order(data.state[, 1], data.state[, 2]), ]
    sorted[1, 2]
}
