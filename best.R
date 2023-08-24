#load tidyverse
library(tidyverse)

#create function
best<-function(state, outcome, num = "best"){
  
  #read csv file
  data <- read.csv("outcome-of-care-measures.csv")
  
  #determine the unique possible states and outcomes
  states <- unique(data[["State"]])
  outcomes <- c("heart attack", 
                "heart failure", 
                "pneumonia")
  
  #validating outcomes and states
  if (!state %in% states) {
    stop('invalid state')
  }
  
  if (!outcome %in% outcomes) {
    stop('invalid outcome')
  }
  
  if ((!outcome %in% outcomes) & (!state %in% states)) {
    stop('invalid state and outcome')
  }
  

  
  #valid function
  if ((outcome %in% outcomes) & (state %in% states)) {
    
    #if user chooses heart attack as an outcome
    if (outcome == "heart attack"){
      
      #recreate dataset to find the minimums for each state
     data2 <- data %>% 
        select('Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 'State') %>% 
        group_by(State) %>% 
        slice(which.min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      
     #return the Hospital name based on the chosen state and the new data
      return(data2$Hospital.Name[data2$State == state])
    
      #if user chooses heart failure as an outcome
    } 
    else if (outcome == "heart failure"){
     
       data2 <- data %>% 
        select('Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 'State') %>% 
        group_by(State) %>% 
        slice(which.min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      
      return(data2$Hospital.Name[data2$State == state])
      
      #if user chooses pneumonia as an outcome
    } 
    else if (outcome == "pneumonia"){
     
       data2 <- data %>% 
        select('Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia', 'State') %>% 
        group_by(State) %>% 
        slice(which.min(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      
      return(data2$Hospital.Name[data2$State == state])
    }
    
  }
  
}
