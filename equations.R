### Script gathering all equations used in the model

#' @title computeCMS
#' @description
#' This function calculates the catch per metier 
#' $$CMS_{sp,m} = biomass_{sp} . q_{sp,m} . effort_{m} . effortScalar_m$$, 
#' or how much biomass is caught by each metier. Here, the effort is 'currentF' 
#' multiplied by the effort scalar in the "metier" columns of 'object_alive'
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
#' This function calculates the freight cost. It sums the species catch per metier 
#' and calculate the freight cost in tonnes (converting from kg) per metier and 
#' effort combinations
#' 
#' @inheritParams computeCMS 
#' 
#' @param CMS Matrix of dimensions no metier x no of species * no of ecologically 
#' viable effort combinations. Obtained with 'computeCMS()'
#' @ecoParam Dataframe of user input costs
#' 
#' @export


computeFRE <- function(CMS, object_alive, ecoParam){
  CMS_ag <- as.data.frame(t(CMS))
  CMS_ag$effortID <- object_alive$effortID
  CMS_ag <- aggregate(. ~ effortID, FUN=sum, data=CMS_ag)
  cFre <- apply(CMS_ag[,-1],1, FUN = function(x) x * ecoParam$cFre * 1000)
  return(cFre)
}

#' @title computeGVL
#' 
#' @description This function uses the CMS and fish price to calculate the gross 
#' value for every metier and effort combinations. It also returns the GVL per 
#' species to produce a table in the "debug" tab. Can be removed if table not necessary anymore
#' 
#' @inheritParams computeFRE
#' 
#' @param fishPrice Dataframe of user input fish price
#' 
#' @export

computeGVL <- function(CMS, object_alive, fishPrice){
  GVL_sp <- apply(t(CMS),2, function(x)x*fishPrice)
  GVL_df <- as.data.frame(GVL_sp)
  GVL_df$effortID <- object_alive$effortID
  # sum species by group of similar effort ID to get a GVL per metier
  GVL <- aggregate(. ~ effortID, FUN=sum, data=GVL_df) 
  return(list(GVL,GVL_df))
}


#' @title computeGOS
#' 
#' @description
#' This function calculates the gross operative surplus of each metier per effort 
#' combination. It first removes the crew share from the GVL, then removes the 
#' additional costs for freight, variable and fixed. Freight costs are calculated 
#' with 'computeFre'. Variable costs come from the number of shot per metier, 
#' inputed by the user. Fixed costs are inputed by the user and scaled down to 
#' take into account that only the Eastern fisheries are modelled here.
#' 
#' @inheritParams computeFRE
#' 
#' @param GVL Dataframe of dimensions no viable effort combinations x no metier + 1 
#' (the first colunm is the effort combination ID). Obtained with 'computeGVL()'
#' @param cFre Matrix of dimensions no metier x no viable effort combinations. 
#' Obtained with 'computeGVL()'
#' @param shotNumber Dataframe of user input. These are the annual variable costs 
#' in AUD at the fleet level 
#' @param vesselNumber Dataframe of user input, no of vessels per metier
#' 
#' @export


computeGOS <- function(GVL, cFre, object_alive, ecoParam,  shotNumber, vesselNumber){
  # crew share
  NVL <- GVL
  NVL[,-1] <- GVL[,-1] * (1 - ecoParam$cShr) 
  
  # freight cost
  NVL[,-1] <- NVL[,-1] - t(cFre)
  
  # variable costs
  rtbs <- apply(as.matrix(NVL),1, 
                function(x) x[2:15] - 
                  (ecoParam$cVarUE * shotNumber * 
                     as.numeric(filter(object_alive, effortID == as.numeric(x[1]))[1,4:17])))
  
  # fixed costs
  GOS <- apply(rtbs,2, function(x) x - ecoParam$cFix * vesselNumber)
  return(GOS)
}

#' @title computeWage
#' @description
#' This function calculate the wages
#' 
#' @inheritParams computeGOS
#'

computeWage <- function(GVL, ecoParam, vesselNumber){
  wage_FTE <-  ecoParam$cShr*GVL[,-1]/(ecoParam$noCrew * vesselNumber)
  wage <- as.data.frame(wage_FTE)
  wage$effortID <- GVL$effortID
  return(wage)
}