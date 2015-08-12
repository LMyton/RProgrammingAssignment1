pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        
       
        # determing which column of data to use
        if (pollutant == "nitrate"){
                pollutantposition <- 3      
                print (paste("pollutant = ", pollutant, " so nitrate 3 used"))
        }
        
        else if (pollutant == "sulfate")
        {
                pollutantposition <- 2    
                print (paste("pollutant = ", pollutant, " so sulphate 2 used"))
        }
        
        #initialise the results set
        allresults <- numeric()
        
        # loop through all ids passed in
        for (i in seq_along(id)) {
                #pad the filename to 3 digits
                fileid <- formatC(id[i],width=3, flag=0)
                
                #determine the filename path
                filenamepath <- paste("C:/Coursera/2RProgramming/",directory,"/",fileid,".csv",sep = "")
                
                # read the contents of the file in
                fileresults <- read.csv(filenamepath)
                
                # output filename and number of rows
                #print (paste(filenamepath, nrow(fileresults)))
                
                
                # append this loops results to allresults
                allresults <-c(allresults,fileresults[,pollutantposition])
                
        
        }
        #calculate the mean (ignoring any NA results)
        mean(allresults,na.rm = TRUE)
}
        
        
        