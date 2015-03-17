best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
        
        decease <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if (state %in% data$State) {
                if (outcome %in% decease){
                        if (outcome == decease[1])
                                col <- 11 
                        else if (outcome == decease[2])
                                col <- 17 
                        else col <- 23
                        
                        ##narrow down the data to selected state
                        statedata <- data[data$State==state,]
                        
                        ##retrieve the exact index for with minimum number 
                        ##of 'col' decease
                        idx <- suppressWarnings(which.min(statedata[,col]))
                        
                        ## Return hospital name in that state with lowest 30-day death
                        ## rate
                        statedata[idx, c(2)]
                }
                else stop("invalid outcome")
        }        
        else stop("invalid state")
}