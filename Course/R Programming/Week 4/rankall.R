rankall <- function(outcome,num="best"){
  ## Read data
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  ## coerce relevant columns stepwise from factors to characters and numeric
  outcome_data[,c(11,17,23)] <- suppressWarnings(lapply(lapply(outcome_data[,c(11,17,23)],as.character),as.numeric))
  
  possibleOutcomes <- c("heart attack","heart failure", "pneumonia")
  
  if (!outcome %in% possibleOutcomes){
    stop("Invalid outcome")
  }
  
  ## rename column names in order to map outcome argument values to column index of dataframe
  colnames(outcome_data)[c(11,17,23)] <- possibleOutcomes
  myMat <- matrix(data=c(11,17,23),nrow=1)
  colnames(myMat) <- possibleOutcomes
  
  rankedDf <- data.frame()
  for (states in unique(outcome_data[,7])){
    outcome_Filter <- outcome_data[outcome_data[,7]==states,c(2,7,myMat[,outcome])]
    
    outcome_Order <- outcome_Filter[order(outcome_Filter[,3], outcome_Filter[,1], na.last=c(T,F)),]
    outcome_Complete <- outcome_Order[complete.cases(outcome_Order),]
    
    if (num=="best"){
      rankedDf <- rbind(rankedDf, outcome_Complete[1,c(1,2)])
    } else if (num=="worst"){
      rankedDf <- rbind(rankedDf, outcome_Complete[nrow(outcome_Complete),c(1,2)])
    } else {
      if (num > nrow(outcome_Complete)){
        nullDf <- data.frame("Hospital.Name"=NA, "State"=outcome_Complete[,c(2)])
        rankedDf <- rbind(rankedDf, nullDf[1,c(1,2)])
      } else {
        rankedDf <-rbind(rankedDf, outcome_Complete[num,c(1,2)])
      }
    }
  }
  return(rankedDf[order(rankedDf[,2]),])
}
