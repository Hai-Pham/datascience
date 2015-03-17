rankhospital <- function(state, outcome, num = "best") {
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
                        
                        ##sort using the order() function for whole data frame
                        ##do never use sort() as it sort selected col. only
                        sorted <- statedata[order(statedata[[col]],
                                                  decreasing=F) , ]
                        
                        ##process the output
                        if (num == "best") 
                                sorted[1, c(2)]
                        else if (num == "worst") 
                        {
                                x <- sorted[[col]]
                                idx <- suppressWarnings(which.max(x))
                                sorted[idx, c(2)]
                        }
                                                
                        else
                                sorted[num, c(2)]
                }
                else stop("invalid outcome")
        }        
        else stop("invalid state")

}