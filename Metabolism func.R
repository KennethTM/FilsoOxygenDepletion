#Funktion oprindeligt fra Gribskov project
#Funktioner til beregning af metabolism
#Returneres: GPP, R og NEP, plot af dopred vs doobs med cor værdi
#Input: DateTime_UTC, doobs, dosat, kgas, zmix, lux, wtr, dummy

#NLL funktion med GPP Jassby/Platt og Arhenius/Jørgensen R
nllfn <- function (pars, datain) {
  
  Pmax <- exp(pars[1])
  Rmax <- exp(pars[2])
  DOInit <- exp(pars[3])
  #alpha <- exp(pars[3])
  
  #definer variable fra datain frame
  nobs <- dim(datain)[1]
  irr <- datain$lux
  doobs <- datain$doobs
  k.gas <- datain$kgas
  Rwtr <- datain$wtr
  zmix <- datain$zmix
  dummy <- datain$dummy
  dosat <- datain$dosat
  
  #Set up output
  dohat <- rep(NA,nobs)
  atmflux <- rep(NA,nobs)
  #dohat[1] <- datain$doobs[1]
  dohat[1] <- DOInit
  
  #metabolisme model
  for (i in 1:(nobs-1)) {
    atmflux[i] <- dummy[i] * -k.gas[i] * (dohat[i] - dosat[i]) / zmix[i]  
    #dohat[i+1] <- dohat[i] + (Pmax*tanh(alpha*irr[i]/Pmax)) - (Rmax*1.073^(Rwtr[i]-20)) + atmflux[i]
    dohat[i+1] <- dohat[i] + (Pmax*irr[i]) - (Rmax*1.073^(Rwtr[i]-20)) + atmflux[i]
  }
  
  #beregning af residualer/sigma2 samt negative log likelihood
  res <- doobs - dohat
  nres <- length(res)
  SSE <- sum(res^2)
  sigma2 <- SSE/nres
  NLL <- -sum(dnorm(doobs, dohat, sd=sqrt(sigma2), log=TRUE)) 
  return(NLL)
}

#dopred
domodel <- function(pars, datain) {
  
  nobs <- dim(datain)[1]
  irr <- datain$lux
  doobs <- datain$doobs
  k.gas <- datain$kgas
  Rwtr <- datain$wtr
  zmix <- datain$zmix
  dummy <- datain$dummy
  dosat <- datain$dosat
  
  dopred <- rep(NA,nobs)
  atmflux <- rep(NA,nobs)
  dopred[1] <- pars$doinit
  #dopred[1] <- datain$doobs[1]
  
  for (i in 1:(nobs-1)) {
    atmflux[i] <- dummy[i] * -k.gas[i] * (dopred[i] - dosat[i]) / zmix[i]  
    #dopred[i+1] <- dopred[i] + (pars$gppcoef*tanh(pars$acoef*irr[i]/pars$gppcoef)) - (pars$rcoef*1.073^(Rwtr[i]-20)) + atmflux[i]
    dopred[i+1] <- dopred[i] + (pars$gppcoef*irr[i]) - (pars$rcoef*1.073^(Rwtr[i]-20)) + atmflux[i]
  }
  
  return(dopred)
}

#samle funktion
metab_calc <- function(df){
  datain <- df
  
  #parguess <- log(c(1.7E-8, 4.5E-3, 1E-5))
  parguess <- log(c(2.15E-5, 7E-6, datain$doobs[1])) #1.3E-4
  
  fit <- tryCatch(optim(parguess, nllfn, datain = datain, method = "Nelder-Mead"), error = function(err){NULL}) #BFGS
  if(is.null(fit)){return(NA)}
  
  gppcoef <- exp(fit$par[1])
  rcoef <- exp(fit$par[2])
  doinit <- exp(fit$par[3])
  #acoef <- exp(fit$par[3])
  convergence <- fit$convergence
  
  #GPP <- mean(gppcoef*tanh(acoef*datain$lux/gppcoef))*144
  GPP <- mean(gppcoef*datain$lux)*144
  R <- mean(rcoef*1.073^(datain$wtr-20))*144
  NEP <- GPP - R
  
  #pars <- list(gppcoef = gppcoef, rcoef = rcoef, acoef = acoef) 
  pars <- list(gppcoef = gppcoef, rcoef = rcoef, doinit = doinit) 
  dopred <- domodel(pars = pars, datain = datain)
  r_spear <- cor(dopred, datain$doobs, method = "spearman")
  rmse <- sqrt(mean((datain$doobs-dopred)^2))

  return(list(data.frame(DateTime_UTC_min = min(datain$DateTime_UTC), DateTime_UTC_max = max(datain$DateTime_UTC),
                                       gppcoef = gppcoef, rcoef = rcoef, doinit = doinit, convergence = convergence,
                                       #gppcoef = gppcoef, rcoef = rcoef, convergence = convergence,
                                       GPP = GPP, R = R, NEP = NEP, r_spear = r_spear, rmse = rmse),
              data.frame(DateTime_UTC = datain$DateTime_UTC, dopred = dopred, doobs = datain$doobs)))
  
}

#dopred
doforecast <- function(meta_result, doinit, datain) {
  
  nobs <- dim(datain)[1]
  irr <- datain$lux
  k.gas <- datain$kgas
  Rwtr <- datain$wtr
  zmix <- datain$zmix
  dummy <- datain$dummy
  dosat <- datain$dosat
  
  dopred <- rep(NA,nobs)
  atmflux <- rep(NA,nobs)
  dopred[1] <- doinit

  for (i in 1:(nobs-1)) {
    atmflux[i] <- dummy[i] * -k.gas[i] * (dopred[i] - dosat[i]) / zmix[i]  
    dopred[i+1] <- dopred[i] + (meta_result$gppcoef*irr[i]) - (meta_result$rcoef*1.073^(Rwtr[i]-20)) + atmflux[i]
  }
  
  return(data.frame(DateTime_UTC = datain$DateTime_UTC, dopred = dopred))
}
