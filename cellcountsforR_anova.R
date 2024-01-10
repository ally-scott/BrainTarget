cat("\f") #clear console


library(readxl)
library(tidyverse)
library(emmeans)
library(lmerTest)

setwd("G:/Shared drives/Richard Lab/Papers and Abstracts/Papers in Progress/Alexandra Scott/GADVPFP/Figs/fig1")

rm(list = ls())
cellcount <- read.csv("cellcountsforR.csv")
cellcount <- cellcount %>%
  mutate(rat = parse_number(`ratID`)) %>% 
  select(rat, cellID, cellperc)


# run linear mixed effect models
###
cellperc_model <- lmer(cellperc ~ cellID + (1 | rat), data = cellcount)
cellperc_lme<-summary(cellperc_model)
cellperc_lmeanova<- anova(cellperc_model)
capture.output(cellperc_lme, file = "cellperc_lme_results.doc")
capture.output(cellperc_lmeanova, file = "cellperc_lmeanova_results.doc")


means<-cellcount %>%
  group_by(cellID) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),cellperc)



meanswinsubj<-cellcount %>%
  group_by(cellID,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),cellperc)


## follow up comparisons 
em <- emmeans(cellperc_model,~ cellID)
GadGFP <- c(1, 0, 0, 0)
GADGlutGFP <- c(0, 1, 0, 0)
GlutGFP<- c(0, 0, 1, 0)
GFP <- c(0, 0, 0, 1)


contrast(em, method = list("GadGFP - GADGlutGFP" = GadGFP - GADGlutGFP,
                           "GadGFP - GlutGFP" = GadGFP - GlutGFP,
                           "GadGFP - justGFP" = GadGFP - GFP))

##Plot
library(ggplot2)



ggplot(meanswinsubj, aes(x=as.factor(cellID), y=mean )) + 
  stat_summary(fun.data = mean_se,geom = "errorbar")+      
  stat_summary(fun = "mean", geom = "bar")+
  #stat_summary(fun = "mean", geom = "bar")+
  geom_point(aes(size = 3))+
  #scale_y_continuous(limits = c(0,12),breaks = seq(from = 0, to = 12, by = 2))+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) + 
  labs(title="GFP+ cell counts_JReditonHALO",x ="Cell ID", y = "Percentage of GFP+ cells ")  
ggsave("GCaMP6cellcounts_gadglut_withpointsdff.pdf")


