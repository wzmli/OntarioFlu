library(depmixS4)
library(tidyverse)
library(ggplot2)

delay_counts <- read.csv("FullDelayCounts.csv")
cutoff <- 30

fitfun <- function(x){
model <- depmixS4:::mix(longdat~1
                        , data=x
                        , nstate=4
                        , family=Gamma("log")
                        , respstart = c(1,2,3,4)
                        , prior=~1
)
ff <-  depmixS4:::fit(model,emcontrol=em.control(maxit=500))
return(ff)
}

dpoismix <- function(x,ff){
  pars <- getpars(ff)
  prob <- head(pars,length(pars)/2)
  lams <- exp(tail(pars,length(pars)/2))
  return(sum(prob*sapply(lams,function(y)dpois(x,lambda=y))))
}


mpfit <- (delay_counts
   %>% dplyr:::select(-X)
   %>% filter(Type %in% c("Admission to Discharge", "Service to Discharge", "Admission to Mortality"))
   %>% mutate(Diagnosis = factor(Diagnosis, levels=c("Influenza", "Pneumonia", "Other Respiratory")))
   %>% filter(Diffdays <= cutoff)
   %>% mutate(Counts = floor(as.integer(Counts)))
   %>% filter(!is.na(Counts))
   %>% group_by(Pandemic, Diagnosis, Type)
   %>% nest()
   %>% mutate(repdat = map(data,~data.frame(longdat=rep(.[["Diffdays"]],times=.[["Counts"]])))
        , mixfit = map(repdat,~fitfun(.x)
        )
   )
)

mppred <- (mpfit
   %>% group_by(Pandemic, Diagnosis, Type)
   %>% mutate(days = map(data,~.x[["Diffdays"]])
     , pred = map(.x=days,.y=mixfit, ~sapply(.x,function(a)dpoismix(a,.y[[1]])))
      )
   %>% unnest(data,pred)
   %>% group_by(Type, Diagnosis, Pandemic)
   %>% mutate(dens = Counts/sum(Counts))
)
