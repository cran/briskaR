# @description \code{brk_toxFun_damage1} toxicokinetic model given by:
# 
# \code{damage(t+1) = kin * exposure - kout * damage(t)} 
#  
# \code{brk_toxFun_survival1} - toxicodynamic model given by:
# 
# \code{prob_survival(t+1) = prob_survival(t) * rbinom(n=1, size=1, prob = alpha1 * exp(-alpha2 * damage)^alpha3)} 
# 
# \code{brk_toxFun_survival2} - logistic model given by:
# 
# \code{prob_survival = 1/(1+(x/LC50)^(-slope))} 
#  
# @description some models for toxicokinetic toxicodynamic

#' @name brk_toxFun
#' 
#' @title Functions for Toxicokinetic-Toxidynamic (TKTD) models
#' 
#' 
#' @param exposure exposure level of individual
#' @param kin parameter describing the intake rate of the element
#' @param kout parameter describing the excretion rate of the element
#' 
#' @export
#' 
brk_toxFun_damage1 = function(exposure, kin, kout){
  damage = c()
  damage[1] = 0
  if(length(exposure) >1){
    for(i in 2:length(exposure)){
      damage[i] = (1-kout) * damage[i-1] + kin * exposure[i]
    }
  }
  return(damage)
}


#' @name brk_toxFun
#'  
#' @param damage damage level on which the survival model is going to be applied
#' @param alpha1 parameter describing the natural background death mortality rate
#' @param alpha2 parameter describing the killing rate of the element on individual
#' @param alpha3 exponential parameter describing the killing rate of the element on individual
#' 
#' @export
#' 
brk_toxFun_survival1 = function(damage, alpha1, alpha2, alpha3){
  psurv = c()
  psurv[1] = 1
  if(length(damage)>1){
    for(i in 2:length(damage)){
      psurv[i] = min(psurv[i-1], 1) * rbinom(n=1, size=1, prob = alpha1 * exp(-alpha2*damage[i])^alpha3) 
    }
  }
  return(psurv)
}


#' @name brk_toxFun
#' 
#' @param LC50 parameter describing the lethal concentration for 50\% of the population
#' @param slope parameter describing the slope of the curve
#' 
#' @export
#' 
brk_toxFun_survival2 = function(damage, LC50, slope){
  return(1/(1+(damage/LC50)^(-slope)))
}


# Internal function
#
# @param t time
# @param State State variable
# @param parms list of parameters
# @param input input exposure profile
model_IT <- function(t, State, parms, input) {
  with(as.list(c(parms, State)), {
    conc_ext <- input(t)
    D = State[1:mcmc_size]
    dD <- kd*(conc_ext - D)    # internal damage
    list(dD = dD, signal = conc_ext)
  })
}

#' ODE solver applied to IT-GUTS model
#' 
#' @name brk_toxFun
#' 
#' @param time vector of time of the exposure profile
#' @param Cw vector of concentration of the exposure profile
#' @param listParameters A list of parameter for the IT model
#' 
#' @import deSolve
#' @importFrom deSolve ode
#'
#' @export
#'
brk_survIT <- function(time, Cw, listParameters){
  signal <- data.frame(times = time,
                       import = Cw)
  
  sigimp <- stats::approxfun(signal$times,
                             signal$import,
                             method = "linear",
                             rule = 2)
  
  times <- seq(min(time), max(time), length = 100)
  
  ## values for steady state
  xstart <- c(D = rep(0, listParameters$mcmc_size))
  ## model
  out <- deSolve::ode(y = xstart,
             times = times,
             func = model_IT,
             parms = listParameters,
             input = sigimp)
  
  D <- out[, grep("D", colnames(out))]
  
  cumMax_D <- apply(D, 2, cummax)
  
  dtheo <- ( 1 - t(1 / (1 + (t(cumMax_D) / listParameters$alpha)^(-listParameters$beta))) ) * exp(times %*% t(-listParameters$hb))
  
  return(dtheo)
}


# Internal SD function
#
# @param t time
# @param State State variable
# @param parms list of parameters
# @param input input exposure profile
model_SD <- function(t, State, parms, input)  {
  with(as.list(c(parms, State)), {
    
    conc_ext = input(t)
    
    D = State[1:mcmc_size]
    H = State[(mcmc_size+1):(2*mcmc_size)]
    
    dD <- kd * (conc_ext - D)     # internal concentration
    dH <- kk * pmax(D - z, 0) + hb # risk function
    
    res <- c(dD, dH)
    list(res, signal = conc_ext)
  })
}

#' ODE solver applied to SD-GUTS model
#' 
#' @name brk_toxFun
#' 
#' @param time vector of time of the exposure profile
#' @param Cw vector of concentration of the exposure profile
#' @param listParameters A list of parameter for the SD model
#'
#' @export
#'
brk_survSD  <- function(time, Cw, listParameters){
  signal <- data.frame(times = time,
                       import = Cw)
  
  sigimp <- stats::approxfun(signal$times,
                             signal$import,
                             method = "linear",
                             rule = 2)
  
  times <- seq(min(time), max(time), length = 100)
  
  ## values for steady state
  xstart <- c(D = rep(0, listParameters$mcmc_size),
              H = rep(0, listParameters$mcmc_size))
  ## model
  out <- deSolve::ode(y = xstart,
             times = times,
             func = model_SD,
             parms = listParameters,
             input = sigimp)
  
  dtheo <- exp(- out[, grep("H", colnames(out))] )
  
  return(dtheo)
}


