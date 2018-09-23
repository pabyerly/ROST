library(RMark)
data(mstrata)

run.mstrata=function()
{
#
# Process data
#
mstrata.processed=process.data(mstrata,model="Multistrata")
#
# Create default design data
#
mstrata.ddl=make.design.data(mstrata.processed)
#
#  Define range of models for S; note that the betas will differ from the output
#  in MARK for the ~stratum = S(s) because the design matrix is defined using
#  treatment contrasts for factors so the intercept is stratum A and the other
#  two estimates represent the amount that survival for B abd C differ from A.
#  You can use force the approach used in MARK with the formula ~-1+stratum which
#  creates 3 separate Betas - one for A,B and C.
#
S.stratum=list(formula=~stratum)
S.stratumxtime=list(formula=~stratum*time)
#
#  Define range of models for p
#
p.stratum=list(formula=~stratum)
#
#  Define range of models for Psi; what is denoted as s for Psi
#  in the Mark example for Psi is accomplished by -1+stratum:tostratum which
#  nests tostratum within stratum.  Likewise, to get s*t as noted in MARK you
#  want ~-1+stratum:tostratum:time with time nested in tostratum nested in
#  stratum.
#
Psi.s=list(formula=~-1+stratum:tostratum)
Psi.sxtime=list(formula=~-1+stratum:tostratum:time)
#
# Create model list and run assortment of models
#
model.list=create.model.list("Multistrata")
#
# Add on specific models that are paired with fixed p's to remove confounding
#
p.stratumxtime=list(formula=~stratum*time)
p.stratumxtime.fixed=list(formula=~stratum*time,fixed=list(time=4,value=1))
model.list=rbind(model.list,c(S="S.stratumxtime",p="p.stratumxtime.fixed",
  Psi="Psi.sxtime"))
model.list=rbind(model.list,c(S="S.stratum",p="p.stratumxtime",Psi="Psi.s"))
#
# Run the list of models
#
mstrata.results=mark.wrapper(model.list,data=mstrata.processed,ddl=mstrata.ddl,threads=2)
#
# Return model table and list of models
#
return(mstrata.results)
}
mstrata.results=run.mstrata()
#summary results for list of models run
mstrata.results
#model average estimates of real parameters 
model.average(mstrata.results)
#PIM to show parameter indice
mod1<-mstrata.results$S.stratum.p.stratum.Psi.s
PIMS(mod1,"S",simplified="F")
PIMS(mod1,"p",simplified="F")
PIMS(mod1,"Psi",simplified="F")

# Example of reverse Multistratum model
data(mstrata)
#default "forward"
mod=mark(mstrata,model="Multistrata")
mod.rev=mark(mstrata,model="Multistrata",reverse=TRUE)
# "reverse
Psilist=get.real(mod,"Psi",vcv=TRUE)
Psilist.rev=get.real(mod.rev,"Psi",vcv=TRUE)
Psivalues=Psilist$estimates
Psivalues.rev=Psilist.rev$estimates
Psi<-TransitionMatrix(Psivalues[Psivalues$time==1,])
Psi.rev<-TransitionMatrix(Psivalues.rev[Psivalues.rev$occ==1,])

S<-summary(mod)$reals$S

S<-matrix(data=c(S[[1]]$pim[1],S[[2]]$pim[1],S[[3]]$pim[1]),nrow=3,ncol=1,dimnames=list(c("A","B","C")))


Phi[1,]<-S[1]*Psi[1,]
Phi[2,]<-S[2]*Psi[2,]
Phi[3,]<-S[3]*Psi[3,]
#mod
Psi
S
Phi

##reverse

S.rev<-summary(mod.rev)$reals$S

S.rev<-matrix(data=c(S.rev[[1]]$pim[1,][2],S.rev[[2]]$pim[1,][2],S.rev[[3]]$pim[1,][2]),nrow=3,ncol=1,dimnames=list(c("A","B","C")))


#overall transition
Phi.rev<-matrix(nrow=3,ncol=3,dimnames=list(c("From:A","From:B","From:C"),c("To:A","To:B","To:C")))

Phi.rev[,1]<-S.rev[1]*Psi.rev[,1]
Phi.rev[,2]<-S.rev[2]*Psi.rev[,2]
Phi.rev[,3]<-S.rev[3]*Psi.rev[,3]
#mod
Psi.rev
S.rev
Phi.rev
#Note that Phi.rev and Phi are nearly identical as they should be, since each is just parameterized differently in terms of the
#products of Psi and S

rm(list=ls())
cleanup(ask=FALSE)



