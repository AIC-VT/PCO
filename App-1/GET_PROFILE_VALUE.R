# Submitted to OSEHRA 05/25/2017 by Jihwan Park.
# Original routine authored by Jihwan Park 2017.

# GET_PROFILE_VALUE.R
getProfileValue <- function (...,dfSource,profileCondition,timeCondition=16) {
  retValue = NA
  
  retValue = (round(mean(dfSource[which(dfSource$Time_Point == timeCondition),profileCondition]),2))
  newTimePoint = 14
  while ( is.na(retValue) | retValue == 0 ) {
    retValue = (round(mean(dfSource[which(dfSource$Time_Point == newTimePoint),profileCondition]),2))
    newTimePoint = newTimePoint - 1
  }
  
  return(retValue)
}


# GET_PROFILE_VALUE.R
getNumberofPatients1 <- function (...,dfSource,profileCondition) {
  retValue = NA
  
  retValue = length(unique(dfSource[,profileCondition]))

  return(retValue)
}

getNumberofPatients2 <- function (...,dfSource,profileCondition,timeCondition=1) {
  retValue = NA
  
  retValue = length(unique(dfSource[which(dfSource$Time_Point == timeCondition),profileCondition]))

  return(retValue)
}
