#Script for loading libraries and functions used in the analysis

library(tidyverse);library(lubridate);library(zoo);library(readxl)
library(LakeMetabolizer);library(patchwork);library(ggsci)

Sys.setenv(TZ="GMT")
Sys.setlocale("LC_TIME", "US")

#Aq. Sci.:For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.

theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

event_date <- ymd("2018-07-28")

event <- ymd_hm("2018-07-28 00:00")
event_end <- ymd_hm("2018-08-12 23:50")

#Functions for calculating lake metabolism (MLE)
#Input: DateTime_UTC, doobs, dosat, kgas, zmix, lux, wtr, dummy
#Return: GPP, R og NEP, plot af dopred vs doobs med cor værdi

#NLL funktion med GPP Jassby/Platt og Arhenius/Jørgensen R
nllfn <- function (pars, datain) {
  
  Pmax <- exp(pars[1])
  Rmax <- exp(pars[2])
  DOInit <- exp(pars[3])

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
  dohat[1] <- DOInit
  
  #metabolisme model
  for (i in 1:(nobs-1)) {
    atmflux[i] <- dummy[i] * -k.gas[i] * (dohat[i] - dosat[i]) / zmix[i]  
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

  for (i in 1:(nobs-1)) {
    atmflux[i] <- dummy[i] * -k.gas[i] * (dopred[i] - dosat[i]) / zmix[i]  
    dopred[i+1] <- dopred[i] + (pars$gppcoef*irr[i]) - (pars$rcoef*1.073^(Rwtr[i]-20)) + atmflux[i]
  }
  
  return(dopred)
}

#function collecting metabolism tools
metab_calc <- function(df){
  datain <- df
  
  parguess <- log(c(5E-5, 2E-3, datain$doobs[1])) #1.3E-4 #7E-6
  
  fit <- tryCatch(optim(parguess, nllfn, datain = datain, method = "Nelder-Mead"), error = function(err){NULL}) #BFGS
  if(is.null(fit)){return(NA)}
  
  gppcoef <- exp(fit$par[1])
  rcoef <- exp(fit$par[2])
  doinit <- exp(fit$par[3])
  convergence <- fit$convergence
  
  GPP <- mean(gppcoef*datain$lux)*144
  R <- mean(rcoef*1.073^(datain$wtr-20))*144
  NEP <- GPP - R
  
  pars <- list(gppcoef = gppcoef, rcoef = rcoef, doinit = doinit) 
  dopred <- domodel(pars = pars, datain = datain)
  r_spear <- cor(dopred, datain$doobs, method = "spearman")
  rmse <- sqrt(mean((datain$doobs-dopred)^2))
  
  return(list(data.frame(DateTime_UTC_min = min(datain$DateTime_UTC), DateTime_UTC_max = max(datain$DateTime_UTC),
                         gppcoef = gppcoef, rcoef = rcoef, doinit = doinit, convergence = convergence,
                         GPP = GPP, R = R, NEP = NEP, r_spear = r_spear, rmse = rmse),
              data.frame(DateTime_UTC = datain$DateTime_UTC, dopred = dopred, doobs = datain$doobs)))
}

#dopred
doforecast <- function(meta_result, doinit, datain, gpp_scale = 1, r_scale = 1, f_scale = 1) {
  
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
    dopred[i+1] <- dopred[i] + gpp_scale*(meta_result$gppcoef*irr[i]) - r_scale*(meta_result$rcoef*1.073^(Rwtr[i]-20)) + f_scale*atmflux[i]
  }
  
  return(data.frame(DateTime_UTC = datain$DateTime_UTC, dopred = dopred))
}
