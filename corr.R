## ----------------------------------------------------------------------------------------------------------------
##
## Function:    corr 
## Input:       Pollution data for fine particulate matter (PM2.5) collected at 332 air pollution monitoring 
##              locations in the United States.                
## Output:      Correlation between sulfate and nitrate for monitor locations where the number of completely 
##              observed cases is greater than the specified threshold. 
##                
## ----------------------------------------------------------------------------------------------------------------

source("complete.R")

corr <- function(directory, threshold=0) {
    
    # get number of complete cases per monitor id
    comp_cases <- complete(directory)
    
    # get a list of ids where number of complete cases exceeds threshold
    qual_obs <- comp_cases[comp_cases$nobs > threshold,]
    qual_ids <- qual_obs[,1]

    # vector that will store correlations
    all_cors <- vector("numeric")
    
    if(length(qual_ids) > 0) {
 
        # import associated files
        id_list <- sapply(qual_ids,sprintf,fmt="%03d")
        file_list <- paste(directory,"/",id_list,".csv",sep="")
        ds <- do.call("rbind",lapply(file_list,read.csv,header=TRUE))
    
        # for each id, get correlation between sulfate and nitrate
        for (i in qual_ids) {
            sub <- subset(ds, ID==i)
            new_cor <- cor(sub$sulfate,sub$nitrate,use="complete.obs")
            all_cors <- c(all_cors,new_cor)
        }
    }
    all_cors

}


