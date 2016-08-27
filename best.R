##States in column 7
##Hospital names in column 2
##Heart Attack rates column 11
##Heart Failure rates column 17
##Pneumonia rates column 23

best <- function(state, outcome) {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!is.element(state, outcomes[, 7])) {
    stop("invalid state")
  }
  if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  s <- outcomes[outcomes[, 7] == state, ]
  s[, 17] <- as.numeric(s[, 17])
  s[, 11] <- as.numeric(s[, 11])
  s[, 23] <- as.numeric(s[, 23])
  
  if (outcome == "heart attack") {
    lowhamat <- s[order(s[, 11], s[, 2]),]
    lowhamat[1, 2]
  } else if (outcome == "heart failure") {
    lowhf <- s[order(s[, 17], s[, 2]),]
    lowhf[1, 2]
  } else if (outcome == "pneumonia") {
    lowpn <- s[order(s[, 23], s[, 2]),]
    lowpn[1, 2]
  }
}