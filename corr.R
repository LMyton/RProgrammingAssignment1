corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        #         Write a function that takes a directory of data files and a 
        #         threshold for complete cases and 
        
        #calculates the correlation 
        #         between sulfate and nitrate for monitor locations where the 
        #         number of completely observed cases (on all variables) is
        #         greater than the threshold.
        
        #         The function should return a vector of correlations 
        #         for the monitors that meet the threshold requirement. 
        
        #         If no monitors meet the threshold requirement, 
        #         Then the function should return a numeric vector of length 0.
        
        
        ##print(dir(directory))
        
        #call complete.R to return the number of complete observed cases in each data file
        
        fileObservedCases <- complete(directory)
        
        #loop through the returned dataframe (fileObservedCases) taking only those over the threshold
        
        filesneeded <- fileObservedCases[fileObservedCases$nobs > threshold,1]
        
        
        
     #   print(paste("files needed = ", nrow(filesneeded)))
        
        #initialise result vector - not sure whether neede
        corrresults <- numeric()
        
        if (length(filesneeded > 0)) {
                # there are more than 0 files that meet threshold
                #each of those files needed - load them in - go and get the sulfate and nitrate values
                
                for (i in seq_along(filesneeded)) {
                        #pad the filename to 3 digits
                        fileid <- formatC(filesneeded[i],width=3, flag=0)
                        
                        #determine the filename path
                        filenamepath <- paste("C:/Coursera/2RProgramming/",directory,"/",fileid,".csv",sep = "")
                        
                        # read the contents of the file in
                        fileresults <- read.csv(filenamepath)
                        
                        # only interested in the non NA ones
                        goodfileresults <- fileresults[complete.cases(fileresults),]
                        
                        filecorr <- cor(goodfileresults[,2], goodfileresults[,3])
                        corrresults <- c(corrresults,filecorr)
#                         print(paste("file=",fileid, 
#                                     " good results = ", nrow(goodfileresults),
#                                     " length corrresult  = ",length(corrresults)))
                        
                }
                
        }
        corrresults
}