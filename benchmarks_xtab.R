library(survey)
library(tigerstats)
library(microbenchmark)

svy_prop <- function(row, column, data, orientation = NULL) {
  
  formula <- as.formula(paste0("~", column, "+", row))
  df <- as.data.frame(survey::svytable(formula, design = data, Ntotal = 100))
  df_prop <- as.data.frame(tapply(df[, 3], list(df[, 2], df[, 1]), mean))
  
  if (missing(orientation)) {
    
    df_prop <- df_prop
    
  }
  
  else {
    
    if (orientation == "colwise") {
      
      df_prop <- apply(df_prop, 2, function(x) {x / sum(x)})
      df_prop[is.na(df_prop)] <- 0
      df_prop <- df_prop * 100
      
    }
    
    else if (orientation == "rowwise") {
      
      df_prop_sum <- apply(df_prop[, 1:ncol(df_prop)], 1, sum)
      df_prop <- df_prop / df_prop_sum
      df_prop[is.na(df_prop)] <- 0
      df_prop <- df_prop * 100
      
    }
    
    else {
      
      df_prop <- df_prop
      
      print("Please select either rowwise or colwise for orientation.")
      
    }
    
  }
  
  return(df_prop)
  
}

svy_prop_2 <- function(row, column, data, orientation = NULL) {
  
  formula <- as.formula(paste0("~", column, "+", row))
  df <- as.data.frame(survey::svytable(formula, design = data, Ntotal = 100))
  df_prop <- as.data.frame(tapply(df[, 3], list(df[, 2], df[, 1]), mean))
  
  if (missing(orientation)) {
    
    df_prop <- df_prop
    
  }
  
  else {
    
    if (orientation == "colwise") {
      
      df_prop <- apply(df_prop, 2, function(x) {x / sum(x)}) * 100
      df_prop[is.na(df_prop)] <- 0
      
    }
    
    else if (orientation == "rowwise") {
      
      df_prop_sum <- apply(df_prop[, 1:ncol(df_prop)], 1, sum)
      df_prop <- df_prop / df_prop_sum
      df_prop[is.na(df_prop)] <- 0
      df_prop <- df_prop * 100
      
    }
    
    else {
      
      df_prop <- df_prop
      
      print("Please select either rowwise or colwise for orientation.")
      
    }
    
  }
  
  return(df_prop)
  
}




svy_prop("discrim2a", "raceth", data = survey_object, orientation = "colwise")


tigerstats::colPerc(survey::svytable(~discrim2a+raceth, design = survey_object))

mb <- microbenchmark(
  
  svy_prop("discrim2a", "raceth", data = survey_object, orientation = "colwise"),
  svy_prop_2("discrim2a", "raceth", data = survey_object, orientation = "colwise"),
  tigerstats::colPerc(survey::svytable(~discrim2a+raceth, design = survey_object)),
  
  times = 2000
  
)

ggplot2::autoplot(mb)
