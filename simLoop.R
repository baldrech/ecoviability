### main loop running sim.eff
# loading BareCode_MSPM to get .sim.eff
# loading RcppSimEffort to get the cpp bit for .sim.effort
# the cpp file is in a package format because necessary for parallel run, to send cpp code to workers


source("BareCode_MSPM_new/Tools.R")
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
  
  
  res <- .sim.eff(K = K,
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