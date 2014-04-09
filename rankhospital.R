rankhospital <- function(state, outcome, num = "best") {
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
    data.state <- na.omit(data.state)
    sorted <- data.state[order(data.state[, 1], data.state[, 2]), ]

    if (num == "best") {
        sorted[1, 2]
    } else if (num == "worst") {
        sorted[nrow(sorted), 2]
    } else if (num <= nrow(sorted)) {
        sorted[num, 2]
    } else {
        NA
    }
}
