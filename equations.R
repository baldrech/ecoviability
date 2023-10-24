### Script gathering all equations used in the model

#' @title computeCMS
#' @description
#' This function calculates the catch per metier 
#' $$CMS_{sp,m} = biomass_{sp} . q_{sp,m} . effort_{m} . effortScalar_m$$, 
#' or how much biomass is caught by each metier. Here, the effort is @currentF 
#' multiplied by the effort scalar in the "metier" columns of @object_alive
#' 
#' @param object_alive Dataframe of dimensions no of species * no of ecologically 
#' viable effort combinations x 19. This is the resulting dataframe containing 
#' only effort yielding a satisfactory ecological threshold.
#' @param qflt Matrix of dimensions no of metiers x no of species. This is the 
#' catchability from the input parameter object (for now estimation.MSP$q)
#' @param currentF Vector of the same length as number of metier. This is the most 
#' recent effort recorded in Effort_metier_Ratpack.csv. Here, 2019. It is used to 
#' calculate the modelled effort using the effort scalar
#' 
#' @export

computeCMS <- function(object_alive, qflt, currentF){
  apply(object_alive, 1, function(x) as.numeric(x[1]) *  as.numeric(x[4:17]) * qflt[,x[3]] * currentF)
}


#' @title computeFRE
#' @description
#' This function calculates the freight cost
#' 
#' @inheritParams computeCMS 
#' 
#' @param CMS 
#' @ecoParam
#' 
#' @export


computeFRE <- function(CMS, object_alive, ecoParam){
  
  # need to aggregate species per metier to calculate freight cost per effort combination per metier
  CMS_ag <- as.data.frame(t(CMS))
  CMS_ag$effortID <- object_alive$effortID
  CMS_ag <- aggregate(. ~ effortID, FUN=sum, data=CMS_ag)
  cFre <- apply(CMS_ag[,-1],1, FUN = function(x) x * ecoParam$cFre * 1000) # Freight cost in kg so converting to Tonnes
  return(cFre)
}


