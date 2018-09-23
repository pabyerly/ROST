require(RMark)
require(plotrix)

rost=read.csv("ROST_NestSuccess_RMark_form.csv")
#summary(rost)

# Write a function for evaluating a set of competing models
run.rost=function()
{
  #1. model for constant daily survival rate (DSR)
  Dot=mark(rost,nocc=55,model="Nest",
           model.parameters=list(S=list(formula=~1)))
  
  #2. DSR varies with colony density 
  Density=mark(rost,nocc=55,model="Nest",
           model.parameters=list(S=list(formula=~Dens)))
  
  #3. DSR varies with colony density and Colony N 
          Dens_N=mark(rost,nocc=55,model="Nest",
          model.parameters=list(S=list(formula=~Dens+N)))
          
          
  #4. DSR varies with colony density and Colony N 
          Dens_N=mark(rost,nocc=55,model="Nest",
          model.parameters=list(S=list(formula=~Dens+N)))
          
#Process model
  return(collect.models() )
}

# The next line runs the 9 models above and takes a minute or 2
rost.results=run.rost() 

# view estimated beta for model in R
rost.results$Dot$results$beta 
# view estimated DSR estimate in R
rost.results$Dot$results$real

# Examine table of model-selection results #
export.MARK(rost.results$Dens$data,"ROSTDSR",rost.results,replace=TRUE,ind.covariates="all")
rost.results                        # print model-selection table to screen
options(width=100)                     # set page width to 100 characters
sink("results.table.txt")              # capture screen output to file
print(rost.results)                 # send output to file
sink()                                 # return output to screen
