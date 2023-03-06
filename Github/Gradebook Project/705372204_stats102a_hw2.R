library(dplyr)
library(tidyr)


messy_impute <- function(messy_table, center = "Mean", margin, ...) {
  # Function that imputes NA values with typical values into messy dataframe
  # Args:
  # x: Dataframe with 11 columns
  # y: Character String
  # z: Integer value 
  # Return:
  # Returns the imputed dataframe with same dimensions as x arg
  
  center <- tolower(center)
  if (class(messy_table) == "data.frame") {
    if (center == "mean") {
      center_function <- function(x) mean(x, na.rm = TRUE,...)
    } else if (center == "median") {
      center_function <- function(x) median(x, na.rm = TRUE,...)
    } else {
      stop("Second input must be 'mean' or 'median'")
    }
    
    if (margin == 1) {
      for(i in 1:ncol(messy_table)) {
        messy_table[ , i][is.na(messy_table[ , i])] <- center_function(messy_table[ , i])
      }
      
    } else if (margin == 2) {
      
      for(i in 1:nrow(messy_table)) {
        row <- messy_table[i, ]
        mean_of_row <- center_function(row[!is.na(row) & grepl("Homework", colnames(messy_table))])
        messy_table[i, ][is.na(messy_table[i ,]) & grepl("Homework", colnames(messy_table))] <- mean_of_row
      }
      
      for(i in 1:nrow(messy_table)) {
        row <- messy_table[i, ]
        mean_of_row <- center_function(row[!is.na(row) & grepl("Quiz", colnames(messy_table))])
        messy_table[i, ][is.na(messy_table[i ,]) & grepl("Quiz", colnames(messy_table))] <- mean_of_row
      }
    } else { 
      stop("Third input must be either 1 or 2")
    }
  } else {
    stop("First input must be a dataframe")
  }
  return(messy_table)
  
}




tidy_impute <- function(tidy_data, center="mean", margin, ...) {
  # Function that imputes NA values with typical values into tidy dataframe
  # Args:
  # x: Dataframe or Tibble
  # y: Character String
  # z: Integer value
  # Return:
  # Function returns imputed dataframe 
  
  center <- tolower(center)
  if (center == "mean") {
    center_fn <- function(x) mean(x, na.rm = TRUE, ...)
  } else if (center == "median") {
    center_fn <- function(x) median(x, na.rm = TRUE, ...)
  } else {
    stop("Function only accepts inputs 'mean' and 'median'")
  }
  
  if (margin == 1) {
    tidy_table <- tidy_data %>% group_by(UID, Assignment_Number)
  } else if (margin == 2) {
    tidy_table <- tidy_data %>% group_by(UID, Assignment_Type)
  } else {
    stop("Input must be either 1 or 2")
  }
  
  tidy_table <- tidy_table %>% mutate(Score = if_else(is.na(Score), center_fn(Score), Score))
  tidy_table
}

