#load tidyverse
library(tidyverse)

#create function of state, outcomes, and ranking
rankhospital <- function(state, outcome, num) {
  
  #read .csv file
  data <- read.csv("outcome-of-care-measures.csv")
  
  #create new data frame with selected columns
  df <- as.data.frame(cbind(data$Hospital.Name, data$State,
                            data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                            data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                            data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                      stringsAsFactors = FALSE)
  colnames(df) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  #determine the unique possible states and outcomes
  states <- unique(df[["State"]])
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #validate states and outcomes
  if (!state %in% states) {
    stop('invalid state')
  }
  
  if (!outcome %in% outcomes) {
    stop('invalid outcome')
  }
  
  if ((!outcome %in% outcomes) & (!state %in% states)) {
    stop('invalid state and outcome')
  }
  
  #numeric rank outcomes for given num in function
  if (is.numeric(num)) {
    df_state <- which(df[, "State"] == state) #make input of state equal state in data frame
    df_state2 <- df[df_state,] #filter data by user inputted state
    df_state2[, outcome] <- as.numeric(df_state2[, outcome]) #change class of outcome data to numeric
    ordered_df <- df_state2[order(df_state2[, outcome], df_state2[, "Hospital"]), ] #order specified outcome rate in increasing order
    ordered_df$index <- 1:nrow(ordered_df) #create a new column with the rank of the hospital based on the outcome
    colnames(ordered_df)[6] = "Rank" #rename the new column "Rank"
    return(ordered_df$"Hospital"[num]) #return the name of the Hospital given the user inputted rank
  }
  
  #non-numeric rank outcomes for given num in function
  else if (!is.numeric(num)) {
    
    #num equals worst
    if (num == "worst"){
      #same as above
      df_state <- which(df[, "State"] == state)
      df_state2 <- df[df_state,]
      df_state2[, outcome] <- as.numeric(df_state2[, outcome])
      ordered_df <- df_state2[order(-df_state2[, outcome], df_state2[, "Hospital"]),] #orders data in descending order
      ordered_df$index <- 1:nrow(ordered_df)
      colnames(ordered_df)[6] = "Rank"
      return(ordered_df$"Hospital"[1]) #returns the hospital name for the first number
    }
    
    #same as numeric entry
    else if (num == "best") {
      df_state <- which(df[, "State"] == state)
      df_state2 <- df[df_state,]
      df_state2[, outcome] <- as.numeric(df_state2[, outcome])
      ordered_df <- df_state2[order(df_state2[, outcome], df_state2[, "Hospital"]),]
      ordered_df$index <- 1:nrow(ordered_df)
      colnames(ordered_df)[6] = "Rank"
      return(ordered_df$"Hospital"[1]) #returns the first Hospital
      
    }
    #returns warning message with an invalid num entry
    else {
      stop('invalid num')
    }
  }
  
  
}
