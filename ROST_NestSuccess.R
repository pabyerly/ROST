library(ggplot2)
library(lme4)


cam=read.csv("ROST_camdata.csv")%>%
 filter(is.na(Fledge) == FALSE) #drop rows with NAs
             
cam$AgeStart=as.numeric(cam$AgeStart)
#standardize explanatory values
density=scale(cam$Density)
age=scale(cam$AgeStart)
colonyN=scale(cam$colonyN)

#logistic exposure function for DSR modeling (Shaffer 2004), need to look into further 
logexp <- function(exposure = 1) {
  linkfun <- function(mu) qlogis(mu^(1/exposure))
  linkinv <- function(eta) plogis(eta)^exposure
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>28,.Machine$double.eps,
           exp(eta)/(1+exp(eta))^2)
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

#mixed effects model--effects of colony N and colony on density 
dens=glm(Density~colonyN, data=cam) #is significant
summary(dens)
#look at residuals, look ok-ish 
plot(dens, which=1)
plot(dens, which=2)

#look at points 
ggplot(cam, aes(x = colonyN, y = Density)) +
  geom_point() +
  geom_smooth(method = "lm") 

boxplot(Density~Island, data=cam) #Island prob having effect on results 
ggplot(cam, aes(x = colonyN, y = Density, colour = Island)) +
  geom_point(size = 2) +
  theme_classic() +
  theme(legend.position = "none")

#try with Island as fixed effect, colonyN no longer having an effect on density 
fixed1=lmer(Density~colonyN+(1|Island), data=cam)
summary(fixed1)
#check residsuals
qqnorm(resid(fixed1))
qqline(resid(fixed1))

#build full model to check VIF
full=glm(Fledge~colonyN+Density+AgeStart+Island+Type+Veg+Place+Str_veg+Str_rock, family=binomial(link=logexp(exdat$t)), data=cam)
car::vif(full)
#island and type have super high VIF, remove from modelso that can retain veg 