best <- function(state,outcome){
  ## Read data
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  ## coerce relevant columns from factors to characters and finally numeric
  ## coercing factors to numeric and finding the min retrieves incorrect results
  outcome_data[,c(11,17,23)] <- suppressWarnings(lapply(lapply(outcome_data[,c(11,17,23)],as.character),as.numeric))
  
  ## Validate arguments
  if (!state %in% outcome_data$State){
    stop("Invalid state.")
  } else if (!outcome %in% c("heart attack","heart failure", "pneumonia")){
    stop("Invalid outcome")
  }
  
  ## rename columns
  colnames(outcome_data)[c(11,17,23)] <- c("heart attack","heart failure","pneumonia")
  
  ## map argument values to index numbers as elements in a matrix
  myMat <- matrix(data=c(11,17,23),nrow=1)
  colnames(myMat) <- c("heart attack","heart failure","pneumonia")
  
  ## pass arguments as filter for column names
  outcome_Filter <- outcome_data[outcome_data[,7]==state, c(2,myMat[,outcome])]
  
  ## order alphabetically per handling requirement
  outcome_Order <- outcome_Filter[order(outcome_Filter[,1]),]
  
  ## find the min of the dataframe
  outcome_Min <- outcome_Order[which.min(outcome_Order[,2]),]
  
  print(outcome_Min[[1]], max.levels=0)
}
