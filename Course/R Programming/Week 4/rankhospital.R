rankhospital <- function(state,outcome,num){
  ## Read data
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  ## coerce relevant columns stepwise from factors to characters and numeric
  outcome_data[,c(11,17,23)] <- suppressWarnings(lapply(lapply(outcome_data[,c(11,17,23)],as.character),as.numeric))
  
  possibleOutcomes <- c("heart attack","heart failure", "pneumonia")
  if (!state %in% outcome_data$State){
    stop("Invalid state")
  } else if (!outcome %in% possibleOutcomes){
    stop("Invalid outcome")
  }
  
  ## rename column names in order to map outcome argument values to column index of dataframe
  colnames(outcome_data)[c(11,17,23)] <- possibleOutcomes
  myMat <- matrix(data=c(11,17,23),nrow=1)
  colnames(myMat) <- possibleOutcomes
  
  ## pass arguments as filter for columns
  outcome_Filter <- outcome_data[outcome_data[,7]==state,c(2,myMat[,outcome])]
  
  ## order alphabetically and by rank
  outcome_Order <- outcome_Filter[order(outcome_Filter[,2], outcome_Filter[,1], na.last=T),]
  
  ## Omit NAs
  outcome_Complete <- outcome_Order[complete.cases(outcome_Order),]
  
  ## add rank column
  outcome_Complete[,c("rank")] <- c(1:nrow(outcome_Complete))
  
  ## returning values based on arguments
  if (num=="best"){
    print(outcome_Complete[1,1], max.levels=0)
  } else if (num=="worst"){
    print(outcome_Complete[nrow(outcome_Complete),1], max.levels=0)
  } else if (num > nrow(outcome_Complete)){
    print(NA)
  } else if (num %in% c(1:nrow(outcome_Complete))){
    print(outcome_Complete[num,1], max.levels=0)
  }
}
