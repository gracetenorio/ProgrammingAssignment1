## ----------------------------------------------------------------------------------------------------------------
##
## Function:    pollutantmean 
## Input:       Pollution data for fine particulate matter (PM2.5) collected at 332 air pollution monitoring 
##              locations in the United States.                
## Output:      Mean for a given pollutant (either sulfate or nitrate) across the specified monitor IDs.    
##                
## ----------------------------------------------------------------------------------------------------------------

pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    # select files to be imported based on value of "id"
    id_list <- sapply(id,sprintf,fmt="%03d")
    file_list <- paste(directory,"/",id_list,".csv",sep="")
    
    # import files into one data frame
    ds <- do.call("rbind",lapply(file_list,read.csv,header=TRUE))
    
    # subset the data to include only records with monitor ID "id"
    sub <- subset(ds,ID %in% id)
    
    ## return the mean of the pollutant "pollutant" for all monitors with ID "id", ignoring NA values
    mean(sub[,pollutant],na.rm=TRUE)
    
}



