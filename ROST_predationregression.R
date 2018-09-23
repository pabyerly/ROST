library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(car)
library(MASS)

#note: in nest success dataset replaced U (unknown) with NA for data analysis purposes. Review U's. 
rost=read.csv("ROST_NestSuccess.csv")

#start age is factor, change to integer            
rost$AgeStart=as.numeric(rost$AgeStart) 
#standardize explanatory values
density=scale(rost$Density)
N=scale(rost$colonyN)
st_veg=scale(rost$Str_veg)
st_rock=scale(rost$Str_rock)
DA=scale(rost$DaysActive)

#Fit models 2: model of pred risk as function of colony size and density, or nest type/veg
pred1 <- glmer(Predation ~ density+N+(1|Cam)+(1|Island), family=binomial, data=rost)
pred2 <- glmer(Predation ~ density+(1|Cam)+(1|Island), family=binomial, data=rost) 
pred3 <- glmer(Predation ~ N+(1|Cam)+(1|Island), family=binomial, data=rost) 
pred4 <- glmer(Predation ~ Place+st_veg+st_rock+(1|Cam)+(1|Island), family=binomial, data=rost)
pred5 <- glmer(Predation ~ density+N+Type+(1|Cam)+(1|Island), family=binomial, data=rost)
pred6 <- glmer(Predation ~ Type+(1|Cam)+(1|Island), family=binomial, data=rost)
anova(pred1, pred2, pred3, pred4, pred5, pred6)

#Fit models 3: model of abandonment as function of colony size and density, or nest type/veg
#Again, need to look up a null model? Or just use model as hypothesis test: say all models suck 
a1 <-glmer(Abandon~ density+N+(1|Cam)+(1|Island), family=binomial, data=rost)
a2 <-glmer(Abandon~ density+(1|Cam)+(1|Island), family=binomial, data=rost)
a3 <-glmer(Abandon ~ N+(1|Cam)+(1|Island), family=binomial, data=rost) 
a4 <-glmer(Abandon~ Place+st_veg+st_rock+(1|Cam)+(1|Island), family=binomial, data=rost)
anova(a1, a2, a3, a4)