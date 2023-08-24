rankall <- function(outcome, num = "best") {
  
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  states <- unique(data[["State"]])
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  df <- as.data.frame(cbind(data$Hospital.Name, data$State,
                            data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                            data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, 
                            data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                      stringsAsFactors = FALSE)
  
  colnames(df) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  suppressWarnings(df$"heart attack" <-as.numeric(df$"heart attack"))
  suppressWarnings(df$"heart failure" <-as.numeric(df$"heart failure"))
  suppressWarnings(df$"pneumonia" <-as.numeric(df$"pneumonia"))
  
 
  
  ##Chek that state and outcome are valid 
  if (!outcome %in% outcomes) {
    stop('invalid outcome')
  }
  
  ##For each state, find the hospital of the given rank 
  
  if (outcome  %in% outcomes){
  #create data frame (ops = outcome per state)
    if (!is.numeric(num)){
      if (num == "best") {
        ordered_df <- df[order(df$State, df[,outcome]), ]
        num_outcomes <- ordered_df %>% group_by(State) %>%  slice(1)
        df_no <- num_outcomes %>% select("Hospital", "State")
        output <- df_no
      }
      else if (num == "worst") {
        ordered_df <- df[order(df$State, -df[,outcome]), ]
        min_outcomes <- ordered_df %>% group_by(State) %>%  slice(1)
        df_mino <- min_outcomes %>% select("Hospital", "State")
        output <- df_mino
      }
    }
    else if (is.numeric(num)) {
      ordered_df <- df[order(df$State, df[,outcome]), ]
      max_outcomes <- ordered_df %>% group_by(State) %>%  slice(num)
      df_maxo <- max_outcomes %>% select("Hospital", "State")
      output <- df_maxo
    }
    
    
  
  ##Return a data frame with the hospital names and the 
  ##(abbreviated) state name
  
    return(output) #return the name of the Hospital given the user inputted rank
  }

}
