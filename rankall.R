rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
        decease <- c("heart attack", "heart failure", "pneumonia")
        
        if (! outcome %in% decease) stop("invalid outcome")
        
        
        if (outcome == decease[1])
                col <- 11
        else if (outcome == decease[2])
                col <- 17
        else col <- 23
        
        ##Sort here is important to keep compliant with 
        ## alphabetical of results
        STATES <- sort(unique(data$State))
        name <- character()
        
        ## For each state, find the hospital of the given rank
        for (i in STATES) {
                ##narrow down the data to selected state
                statedata <- data[data$State==i,]
                
                ##sort using the order() function for whole data frame
                ##do never use sort() as it sort selected col. only
                sorted <- statedata[order(statedata[[col]],
                                          decreasing=F) , ]
                
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                if (num == "best") 
                        name[i] <- sorted[1, c(2)]
                else if (num == "worst") 
                {
                        x <- sorted[[col]]
                        idx <- suppressWarnings(which.max(x))
                        name[i] <- sorted[idx, c(2)]
                }
                else 
                        name[i] <- sorted[num, c(2)]

        }
        
        ##Consolidate the result
        data.frame(hospital=name, state=STATES, row.names=STATES)
}