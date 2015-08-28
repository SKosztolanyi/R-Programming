best <- function(state, outcome) {
      
      ## Read outcome data
      hospital.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
      
      ## Check that state and outcome are valid
      validOutcome = c("heart attack","heart failure","pneumonia")
      validState = unique(hospital.data[,7])
      
            if (!outcome %in% validOutcome) {stop("invalid outcome")}
            if (!state %in% validState) {stop("invalid state")}
      
      ## convert outcome name into column name
      fulloutname <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
      outname <- fulloutname[match(outcome,validOutcome)]
      
      ## Return hospital name in that state with lowest 30-day death rate
      data.state <- hospital.data[hospital.data$State==state,]
      lowest <- which.min(as.numeric(data.state[,outname]))
      data.state[lowest,"Hospital.Name"]
}