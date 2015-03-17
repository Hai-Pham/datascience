corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        #source("complete.R")
        
        filenames <- list.files(directory, pattern = "*.csv", full.names = TRUE)
        filecontents <- lapply(filenames, read.csv) #index begins from 1 

        #check complete cases - Part 2 of Assignment
        full <- complete(directory) 
        
        #narrow down by threshold         
        complete <- subset(full, nobs > threshold) 
        
        #id is the row of complete is the selected id of 
        #files satisfied the threshold
        id <- complete[,1]
        
        data <- numeric(length(id))
        #data <- data.frame(id, cor=numeric(length(id))) 
        
        #only deals with selective id
        for (i in 1:length(id)) {
                if (length(id) > 0) {
                        data[i] = cor(filecontents[[complete$id[i]]]$sulfate, 
                                        filecontents[[complete$id[i]]]$nitrate, 
                                        use = "pairwise.complete.obs"
                                        #use = "na.or.complete"
                                      )
                }
        }
        
        #return vector data
        data
}