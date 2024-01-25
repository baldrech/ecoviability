### main loop running sim.eff
# loading BareCode_MSPM to get .sim.eff
# loading RcppSimEffort to get the cpp bit for .sim.effort
# the cpp file is in a package format because necessary for parallel run, to send cpp code to workers


# source("BareCode_MSPM_new/Tools.R")
# remotes::install_github("baldrech/RcppSimEffort")
library(RcppSimEffort)

# function used in the foreach
simLoop <- function(effortMatrix, estimation.MSP, groups, P.prey, E.Fleet)
{
  # from MSP
  K      <- exp(estimation.MSP$Parameters$logK)
  r      <- exp(estimation.MSP$Parameters$logr)
  lambda <- exp(estimation.MSP$Parameters$loglambda)
  qflt   <- estimation.MSP$q
  C.biom  <- estimation.MSP$Report
  
  ## Effort Arrangement
  EFleet <- read.csv(E.Fleet) ## Predators in columns and prey on rows
  order.flt <- colnames(EFleet[, -1])
  Nflts <- length(order.flt)
  
  # setting up multiplier vector for the effort
  mult <- rep(1,length(order.flt))
  # mult   <- mult[1 : length(order.flt)]
  # Effort <- colMeans(tail(EFleet[, -1]), na.rm = TRUE)  * mult
  # predator prey interactions
  Pprey  <- as.matrix(read.csv(P.prey, row.names = 1, stringsAsFactors = FALSE)) ## Predators in columns and prey on rows
  Pprey[is.na(Pprey)] <-0
  main      <- read.csv(groups)
  order.sps <- as.character(main$Code)  ## This will set the species order
  Nsp <- length(order.sps)
  
  Xp = main$Xp
  mu = main$mu
  Eff = main$Efficiency
  
  start <- "Current Estimation" #"At MSY Level" ## it also can be 'Current Estimation'
  
  
  mult   <- effortMatrix[1 : length(order.flt)]
  Effort <- colMeans(tail(EFleet[, -1]), na.rm = TRUE)  * mult
  
  
  res <- sim.eff(K = K,
                 r = r, 
                 qflt = qflt,
                 Time = 5000, 
                 Effort = Effort, 
                 Nsp = Nsp,
                 Nflts = Nflts,
                 p.asym = lambda, 
                 Pprey = Pprey, 
                 Xp = Xp, 
                 mu = mu, 
                 Eff = Eff, 
                 np = 2, 
                 predation = FALSE, 
                 v.noise = TRUE, 
                 step = 0.1, 
                 C.biom = C.biom, 
                 start = start)
  
  names(effortMatrix) <- order.flt
  
  biores <- data.frame("biomass" = res$B3, "production" = res$P3, "species" = order.sps)
  effortOutput <- matrix(rep(effortMatrix, Nsp), nrow = Nsp, byrow = T)
  colnames(effortOutput) <- order.flt
  return(cbind(biores, effortOutput))
}

#' @title ecoviability'
#' @description Find threshold based on sim loop output

ecoviability <- function(vary, qflt, currentF, estimation.MSP,
                         species_weight = 1100, # how important is the survival of one species?
                         metier_weight = 750, # how important is it to not go bankrupt
                         wage_weight = 900, # how important is to pay your employees
                         min_biom_threshold = .75, 
                         min_eco_threshold = -1000000,
                         min_soc_threshold = 1,
                         threshold_df = FALSE
){
  # print(paste("Processing vector:", toString(vary)))
  
  biom0 <- simLoop(effortMatrix = rep(0,14), estimation.MSP = estimation.MSP, groups = groups, P.prey = P.prey, E.Fleet = E.Fleet)$biomass
  res <- simLoop(effortMatrix = vary, estimation.MSP = estimation.MSP, groups = groups, P.prey = P.prey, E.Fleet = E.Fleet)
  res <- res %>% 
    mutate(biom0 = biom0,
           biom_thresh = biomass/biom0,
           effortID = 1)
  ### biomass threshold ----
  biom_threshold <- filter(res, biom_thresh >= min_biom_threshold)
  if(dim(biom_threshold)[1]){
    no_sp <- length(unique(biom_threshold$species)) # how many species survive the treatment? used for duplicates
    biom_score <- no_sp * species_weight
  } else biom_score <- 0
  ### economic threshold ------
  # catch per metier
  CMS <- computeCMS(object_alive = res,
                    qflt = qflt, 
                    currentF = currentF)
  # freight cost
  cFre <- computeFRE(CMS = CMS, 
                     object_alive = res,
                     ecoParam = param_metier)
  
  # gross value per species
  GVL_list <- computeGVL(CMS = CMS, 
                         object_alive = res,
                         fishPrice = param_species$price)
  GVL <- GVL_list[[1]]
  
  # gross operative surplus
  GOS <- computeGOS(GVL = GVL, 
                    cFre = cFre, 
                    object_alive = res,
                    ecoParam = param_metier, 
                    shotNumber = param_metier$no_shot,
                    vesselNumber = param_metier$no_vessel)
  
  GOS <- data.frame("name" = rownames(GOS),
                    "value" = GOS,
                    row.names = NULL)
  no_metier <- dim(filter(GOS, value >= min_eco_threshold))[1]
  
  eco_score <- no_metier * metier_weight
  
  ### wages ----
  wage <- computeWage(GVL = GVL, 
                      ecoParam = param_metier, 
                      vesselNumber = param_metier$no_vessel) %>% 
    dplyr::select(-effortID)
  
  no_wage <- dim(filter(tidyr::gather(wage, key = "variable", value = "value"), 
                        value >= min_soc_threshold))[1]
  
  soc_score <- no_wage * wage_weight
  
  total_score <- biom_score + eco_score + soc_score
  
  if(threshold_df){
    
   sp_df <- data.frame("name" = colnames(qflt),
               "threshold" = res$biom_thresh)
    GOS$wage <- t(wage)
    colnames(GOS)[2] <- "GOS"
    
    return(list(sp_df,GOS))
  } else return(total_score)
}

##' @title Simulation of effort increase
##' @param K Carrying capacity
##' @param r growth rate
##' @param qflt catchability by metier or fleet
##' @param Time total tiem
##' @param Effort Effort
##' @param Nsp Number of species
##' @param Nflts Number of fleets
##' @param p.asym Asymetry parameter
##' @param Pprey Predator prey intereaction
##' @param Xp is the maximum consumption rate (yr-1)
##' @param mu describes the "half-saturation" level which is the prey biomass level at which predation consumption is half of the maximum
##' @param Eff Feeding Efficiency
##' @param np Np from holling type equ
##' @param predation Bolean variable with true or false in case we want to include predation
##' @param v.noise
##' @param step extra time.org step
##' @param C.biom
##' @param start
##' @return a list of biological parameters
##' @author Javier Porobic
sim.eff <- function(K, r, qflt, Time, Effort, Nsp, Nflts, p.asym = NULL, Pprey = NULL, Xp = NULL, mu = NULL, Eff = NULL, np = 2, predation = FALSE, v.noise = TRUE, step = 0.1, C.biom = NULL, start = 'Current Estimation'){
  B         <- sapply(K, function(x){rep(seq(from = 1, to = x, length = Time))})
  B2        <- B
  pred3     <- prey3 <-pred <- prey <- B * 0
  B3        <- B2 * NA
  totCatch3 <- totCatch <- array(0, dim = c(Time, Nsp))
  C3        <- C <- array(0, dim = c(Time, Nsp, Nflts))
  P         <- B * 0
  P3        <- P2 <- B * 0
  ##~~~~~~~
  ## initial condition for the simulation of the current state of the ecosystem
  ##~~~~~~
  if(start == 'At MSY Level'){
    Bini <- K / 2
  } else if(start == 'Current Estimation'){
    Bini <- C.biom$B[nrow(C.biom$B), ]
  }
  B3[1, ] <- Bini
  B.obs   <- C.biom$B
  C.obs   <- C.biom$totCatch
  P.obs   <- C.biom$P
  P       <- B * NA
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ## ~     Calculatin Predation at different levels of predation                           ~ ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  for(sp in 1 : Nsp){
    for(pr in 1 : Nsp){
      ## Calculating biomass removed by predation and competition
      pred[, sp] <- pred[, sp] + (Pprey[sp, pr] * Xp[pr] * B[, pr]) *
        ((B[, sp] / K[sp]) ^ np) / ((mu[sp])^np + (B[, sp] / K[sp]) ^ np)
      ## Calculating biomass added by predation
      prey[, pr] <- prey[, pr] + Eff[pr] * (Pprey[sp, pr] * Xp[pr] * B[, pr]) *
        ((B[, sp] / K[sp]) ^ np) / ((mu[sp])^np + (B[, sp] / K[sp]) ^ np)
    }
  }
  pred[pred < 0]    <- 0
  prey[prey < 0]    <- 0
  pred[is.na(pred)] <- 0
  prey[is.na(prey)] <- 0
  for(sp in 1 : Nsp){
    for(flt in 1 : Nflts){
      if(Effort[flt] == 0) next()
      if(qflt[flt, sp] == 1e-30) next()
      C[, sp, flt]   <- qflt[flt, sp] * Effort[flt] * B[, sp]
      totCatch[, sp]  <- totCatch[, sp] + C[, sp, flt]
    }
  }
  
  for(sp in 1 : ncol(B)){
    P[, sp]          <-  (r[sp] / p.asym[sp]) * B[, sp] * (1 - (B[, sp] / K[sp]) ^ p.asym[sp])
    B2[2 : Time, sp] <-  B2[1 : (Time - 1), sp] + (r[sp] / p.asym[sp]) * B2[1 : (Time - 1), sp] * (1 - (B2[1 : (Time - 1), sp] / K[sp]) ^ p.asym[sp]) - pred[1 : (Time - 1), sp] + prey[1 : (Time - 1), sp] - totCatch[1 : (Time - 1), sp]
    P2[, sp]  <-  (r[sp] / p.asym[sp]) * B2[, sp] * (1 - (B2[, sp] / K[sp]) ^ p.asym[sp]) - pred[, sp] + prey[, sp];
  }
  out3        <- effort_sim(Bini, K, r, qflt, Time, Effort, p.asym, Pprey, Xp, mu, Eff, np, 1)
  out3[[2]]   <- out3[[2]][1 : out3[[6]], ]
  out3[[3]]   <- out3[[3]][1 : out3[[6]], ]
  P3          <- apply(out3[[2]], 2, function(x) mean(tail(x, 20))) ## Average Production
  P3.sd       <- apply(out3[[2]], 2, function(x) sd(tail(x, 20)))   ## Standar deviation of production
  B3          <- apply(out3[[3]], 2, function(x) mean(tail(x, 20))) ## Average Biomass at equilibrium dinamic
  B3.sd       <- apply(out3[[3]], 2, function(x) sd(tail(x, 20)))   ## Standar deviation of Dynamic Biomass
  neg         <- which(P < 0, arr.ind = TRUE)
  if(length(neg) > 0){
    for(pos in unique(neg[, 2])){
      tmp  <- min(neg[neg[, 2] ==  pos, 1])
      P[tmp : nrow(P), pos] <- NA
      B[tmp : nrow(P), pos] <- NA
    }
  }
  neg    <- which(P2 < 0, arr.ind = TRUE)
  if(length(neg) > 0){
    for(pos in unique(neg[, 2])){
      tmp  <- min(neg[neg[, 2] ==  pos, 1])
      P2[tmp : nrow(P2), pos] <- NA
      B2[tmp : nrow(P2), pos] <- NA
    }
  }
  return(list(Biomass = B, Catch = C, Production = P,
              Food = prey, Production2  = P2, Biomass2 = B2, Predation = pred,
              P3 = P3, SD.P = P3.sd, B3 = B3, SD.B = B3.sd,
              B.obs = B.obs, C.obs = C.obs, P.obs = P.obs))
}
