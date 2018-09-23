library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(car)

cam=read.csv("ROST_camdata.csv") %>%
 filter(is.na(Fledge) == FALSE) #drop rows with NAs

#start age is factor, change to integer            
cam$AgeStart=as.numeric(cam$AgeStart)
#standardize explanatory values
density=scale(cam$Density)
age=scale(cam$AgeStart)
colonyN=scale(cam$colony)
strveg=scale(cam$Str_veg)
strrock=scale(cam$Str_rock)

#build full models to check VIF
#remove type, is correlated to Veg. When removed, all VIF fine.
full=glmer(Fledge~colonyN+age+Veg+Place+strveg+strrock+density+(1|Island)+(1|Cam), family=binomial, data=cam)
car::vif(full)

#Build Models:
#full=hypothesis 1: fledging increases with density, colonyN
cor.test(density, colonyN) #just checking and no, not significantly correlated 
mod1=glmer(Fledge~colonyN+density+(1|Cam)+(1|Island), family=binomial, data=cam)
mod2=glmer(Fledge~colonyN+(1|Cam)+(1|Island), family=binomial, data=cam)

#hypothesis 2: predation risk decreases with n, density, veg structure, place
mod3=glmer(Predation~density+(1|Cam)+(1|Island), family=binomial, data=cam)
mod4=glmer(Predation~colonyN+(1|Cam)+(1|Island), family=binomial, data=cam)
mod5=glmer(Predation~colonyN+density+(1|Cam)+(1|Island), family=binomial, data=cam)
mod6=glmer(Predation~strveg+Place+(1|Cam)+(1|Island), family=binomial, data=cam)


