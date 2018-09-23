library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(car)
library(MASS)
library(AICcmodavg)

rost=read.csv("ROST_NestSuccess_logistic_form.csv")

#start age is factor, change to integer            
rost$AgeStart=as.numeric(rost$AgeStart) 
#standardize explanatory values
density=scale(rost$Density)
nestage=scale(rost$AgeStart)
N=scale(rost$colonyN)
strveg=scale(rost$Str_veg)
strrock=scale(rost$Str_rock)

#build full models to check VIF
full=glmer(Fledge~colonyN+Veg+Place+strveg+strrock+density+Type+(1|Island)+(1|Cam), family=binomial, data=rost)
car::vif(full)
#remove type, is correlated to Veg. When removed, all VIF fine.

#logistic0exposure model: Shaffer 2004
#M. Herzog and Ben Bolker are to be credited for the logexp.r custom link function that works quite well.
# Example of R code to fit daily nest survival (DSR) models

logexp <- function(exposure = 1) {
  linkfun <- function(mu) qlogis(mu^(1/exposure))
  linkinv <- function(eta) plogis(eta)^exposure
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>30,.Machine$double.eps, 
           exp(eta)/(1+exp(eta))^2)
    ## OR .Call(stats:::C_logit_mu_eta, eta, PACKAGE = "stats")
  }
  mu.eta <- function(eta) {
    exposure * plogis(eta)^(exposure-1) *
      logit_mu_eta(eta)
  }
  valideta <- function(eta) TRUE
  link <- paste("logexp(", deparse(substitute(exposure)), ")",
                sep="")
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta,
                 name = link),
            class = "link-glm")
}

#LOOK AT pearson's for N and density--why significant when cor = 0.08? (not high)
#Fit models: model of fledge success as function of colony size and densitym also nest type 
mod=list()
mod[[1]] <- glmer(Fledge ~ density+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[2]] <- glmer(Fledge ~ N+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[3]] <- glmer(Fledge ~ density+N+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[4]] <- glmer(Fledge ~ Veg+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[5]] <- glmer(Fledge ~ Veg+Place+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[6]] <- glmer(Fledge ~ strveg+strrock+Place+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[7]] <- glmer(Fledge ~ Place+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[8]] <- glmer(Fledge ~ strveg+strrock+Place+density+N+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
mod[[9]]<- glmer(Fledge ~ Place+density+N+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(mod), sep = " ")
##generate AICc table
aictab(cand.set = mod, modnames = Modnames, sort = TRUE)
#round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)
#compute evidence ratios
evidence(aictab(cand.set=mod, modnames=Modnames))


