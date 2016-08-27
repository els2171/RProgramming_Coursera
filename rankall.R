##States in column 7
##Hospital names in column 2
##Heart Attack rates column 11
##Heart Failure rates column 17
##Pneumonia rates column 23

rankall <- function(outcome, num = "best") {
    outcomes <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
    
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
      stop("invalid outcome")
    }
 
    if (outcome == "heart attack") {
      mat <- outcomes[, c(2, 7, 11)]
    } else if (outcome == "heart failure") {
      mat <- outcomes[, c(2, 7, 17)]
    } else if (outcome == "pneumonia") {
      mat <- outcomes[, c(2, 7, 23)]
    }
    
    final <- na.omit(mat)
    
    matord <- final[order(final[, 2], final[, 3], final[, 1]), ]
    s <- split(matord, matord[,2])
    if (num == "best")  {
      results <- sapply(s, function(elem) elem[1, 1])
    } else if (num == "worst") {
      results <- sapply(s, function(elem) elem[nrow(elem), 1])
    } else results <-sapply(s, function(elem) elem[num, 1])
    df <- data.frame(hospital = results, state = names(results), row.names = names(results))
    df
    
    }