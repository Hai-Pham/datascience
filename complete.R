complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        filenames <- list.files(directory, pattern = "*.csv", full.names = TRUE)
        filecontents <- lapply(filenames[id], read.csv) #index begins from 1 
        
        nobs = numeric(length(id))
       
        for (i in 1:length(id)) {
                nobs[i] <- sum(complete.cases(filecontents[[i]]))
        }
        
        data.frame(id, nobs)
}