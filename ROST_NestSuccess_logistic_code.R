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
#remove type, is correlated to Veg. When removed, all VIF fine.
#full=glmer(Fledge~colonyN+age+Veg+Place+strveg+strrock+density+(1|Island)+(1|Cam), family=binomial, data=rost)
#car::vif(full)

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

##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = mod, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)


#check c-hat for global model 
c_hat(mod[[1]], method="pearson")
modnames=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
#model selection basd on AICc
aictab(cand.set=mod, modnames=Modnames)
#compute evidence ratios
evidence(aictab(cand.set=mod, modnames=Modnames))




anova(fledge1, fledge2, fledge3, fledge4, fledge5, fledge6, fledge7, fledge8, fledge9) 
Modnames<- paste("mod", 1:length(mod), sep = " ")
modnames<-aictab(cand.set=mod,modnames=Modnames,second.ord=TRUE,nobs=NULL,sort=TRUE)

#Fit models: model of HATCH success as function of colony size and densitym also nest type 
#ALL MODELS didn't converge and have very large eigenvalues--why?
fledge1 <- glmer(Hatch ~ density+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
fledge2 <- glmer(Hatch ~ N+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
fledge3 <- glmer(Hatch ~ density+N+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
fledge4 <- glmer(Hatch ~ density+N+Veg+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
fledge5 <- glmer(Hatch ~ density+N+strveg+Place+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
fledge6 <- glmer(Hatch ~ strveg+Place+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
fledge7 <- glmer(Hatch ~ strrock+Place+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
fledge8 <- glmer(Hatch ~ Type+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost )
fledge9 <- glmer(Hatch ~ Type+density+N+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost)
anova(fledge1, fledge2, fledge3, fledge4, fledge5, fledge6, fledge7, fledge8, fledge9)


#unused models: 
#fledge5 <- glmer(Fledge ~ Veg+Place+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost) #Veg + place perfectly correlated
#fledge7 <- glmer(Fledge ~ nestage+(1|Cam)+(1|Island), family=binomial(link=logexp(rost$int)), data=rost) #can't use this because only have start age for nests that either succeed or at least hatch a chick--not a lot of those

