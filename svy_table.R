#' svy_table
#'
#' @param formula formula of variables on which to run xtabulation with format ~var1+var2
#' @param design survey object containing the variables
#' @param Ntotal A population total or set of population stratum totals to normalise to
#' @param orientation A rowwise or colwise orientation to specify
#'
#' @return returns a survey table
#' @export
#'
#' @examples
#' @importFrom tigerstatss colPerc rowPerc
#' @importFrom survey svytable
#' 
svy_table <- function(formula = NULL, design =  NULL, Ntotal = NULL, orientation = NULL) {
  
  svy_tib <- survey::svytable(formula = formula, design = design, Ntotal = Ntotal)
  
  if (!is.null(orientation)) {
    
    if (orientation=="rowwise") {
      
      return(tigerstats::rowPerc(svy_tib))
      
    }
    
    else if (orientation=="colwise") {
      
      return(tigerstats::colPerc(svy_tib))
      
    }
    
  }
  
  else{return(svy_tib)}
  
  
  
}

svy_table(~discrim2a+raceth, design = survey_object)
