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
        
        
        for (i in seq_along(id)) {
                
                #pad the filename to 3 digits
                fileid <- formatC(id[i],width=3, flag=0)
                
                #determine the filename path
                filenamepath <- paste("C:/Coursera/2RProgramming/",directory,"/",fileid,".csv",sep = "")
                
                # read the contents of the file in
                fileresults <- read.csv(filenamepath)
                
                #remove any rows with NA
                onlygood <- fileresults[complete.cases(fileresults),]
                
                #determine count of non-NA rows 
                onlygoodcount <- nrow(onlygood)
                
                #if first record - create the data frame
                #otherwise add rows to it
                if (i == 1) {
                        output  <- data.frame (id = id[i], nobs = onlygoodcount)
                }
                else {
                        output <- rbind(output, c(id[i],onlygoodcount))
                }
                
                
        }
        
        output
}      

