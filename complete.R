## ----------------------------------------------------------------------------------------------------------------
##
## Function:    complete 
## Input:       Pollution data for fine particulate matter (PM2.5) collected at 332 air pollution monitoring 
##              locations in the United States.                
## Output:      Number of complete cases for the specified monitor IDs.    
##                
## ----------------------------------------------------------------------------------------------------------------

complete <- function(directory, id = 1:332) {

    # select files to be imported based on value of "id"
    id_list <- sapply(id,sprintf,fmt="%03d")
    file_list <- paste(directory,"/",id_list,".csv",sep="")
    
    # import files into one data frame
    ds <- do.call("rbind",lapply(file_list,read.csv,header=TRUE))
    
    # create a new variable that indicates if given observation is complete
    ds["is_complete"] <- complete.cases(ds)
 
    # aggregate number of complete cases for each id
    temp <- aggregate(ds$is_complete, by=list(ds$ID), sum)
    
    # format output dataframe
    result <- temp[match(id,temp$Group.1),]
    row.names(result) <- 1:length(id)
    colnames(result) <- c("id","nobs")
    result

}



