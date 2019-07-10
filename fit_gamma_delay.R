library(tidyverse)
library(ggplot2)
library(glmmTMB)

### purrring 

delay_counts <- read.csv("FullDelayCounts.csv")
cutoff <- 30

gammafit <- (delay_counts
       %>% dplyr:::select(-X)
       %>% filter(Type %in% c("Admission to Discharge", "Service to Discharge", "Admission to Mortality"))
       %>% mutate(Diagnosis = factor(Diagnosis, levels=c("Influenza", "Pneumonia", "Other Respiratory")))
       %>% filter(Diffdays <= cutoff)
       %>% mutate(Counts = floor(as.integer(Counts)))
       %>% filter(!is.na(Counts))
       %>% group_by(Pandemic, Diagnosis, Type)
       %>% nest()
       %>% mutate(repdat = map(data,~rep(.[["Diffdays"]],times=.[["Counts"]]))
        , tmbfit = map(repdat,~glmmTMB(.x ~ 1, family = Gamma(link="log")))
        , gpars = map(tmbfit,~exp(.x[["fit"]][["par"]]))
        , gshape = map_dbl(gpars,~.x[[2]])
        , gscale = map_dbl(gpars,~.x[[1]]/.x[[2]])
       )
)

gammapred <- (gammafit
   %>% unnest(data)
   %>% group_by(Type, Diagnosis, Pandemic)
   %>% mutate(pred = dgamma(Diffdays, shape=gshape, scale=gscale)
              , dens = Counts/sum(Counts)
              )
)
