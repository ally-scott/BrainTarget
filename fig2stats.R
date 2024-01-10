cat("\f") #clear console


library(readxl)
library(tidyverse)
library(emmeans)
library(lmerTest)

setwd("G:/Shared drives/Richard Lab/Papers and Abstracts/Papers in Progress/Alexandra Scott/GADVPFP/Figs/fig2")

rm(list = ls())
stage1to4<- read.csv("GADVPFPallsatgesDSProb.csv")
stage1to4['cue']='DS'
stage1to4 <- stage1to4 %>%
  filter(stage<5)%>%
  mutate(rat = parse_number(`subj`),
         PEprob=mean_y,
         day=mean_x,
         stage=stage,
         stageday=stageday) %>% 
  select(rat, PEprob,day,stageday,stage)

stage1to4<-stage1to4%>%
  group_by(day,rat) %>%
  summarise_each(funs(mean),PEprob)

stage1to4<-subset(stage1to4,rat!=3&8)
stage1to4 <- na.omit(stage1to4)

meanPEstage1to4<-stage1to4%>%
  group_by(day) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),PEprob)

# run linear mixed effect models
###
PEprob1to4_model <- lmer(PEprob ~ day + (1 | rat), data = stage1to4)
PEprob1to4_lme<-summary(PEprob1to4_model)
PEprob1to4_lmeanova<- anova(PEprob1to4_model)
capture.output(PEprob1to4_lme, file = "PEprob1to4_lme_results.doc")
capture.output(PEprob1to4_lmeanova, file = "PEprob1to4_lmeanova_results.doc")

##stage 5 DS/NS

stage5DS<- read.csv("GADVPFPstage5DSProb.csv")
stage5DS['cue']='DS'
stage5NS<- read.csv("GADVPFPstage5NSProb.csv")
stage5NS['cue']='NS'
stage5DSNS<-rbind(stage5DS,stage5NS)

stage5DSNS <- stage5DSNS %>%
  filter(stage==5)%>%
  mutate(rat = parse_number(`subj`),
         PEprob=mean_y,
         day=last10days,
         stage=stage,
         stageday=stageday,
         cue=cue) %>% 
  select(rat, PEprob,day,stageday,stage,cue)

stage5DSNS<-stage5DSNS%>%
  group_by(day,rat,cue) %>%
  summarise_each(funs(mean),PEprob)

stage5DSNS<-subset(stage5DSNS,rat!=3&8)
stage5DSNS <- na.omit(stage5DSNS)

meanPEstage5DSNS<-stage5DSNS%>%
  group_by(day,cue) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),PEprob)

# run linear mixed effect models
###        
PEprob5_model <- lmer(PEprob ~ day*cue + (1 | rat), data = stage5DSNS)
PEprob5_lme<-summary(PEprob5_model)
PEprob5_lmeanova<- anova(PEprob5_model)
capture.output(PEprob5_lme, file = "PEprob5_lme_results.doc")
capture.output(PEprob5_lmeanova, file = "PEprob5_lmeanova_results.doc")


## mean and se for criteria day probability and latency
stage5DSNSPProb<- read.csv("GADVPFPcriteriadayDSNSProb.csv")
stage5DSNSPLat<- read.csv("GADVPFPcriteriadayDSNSLat.csv")


stage5DSNSPProb <- stage5DSNSPProb %>%
  mutate(rat = parse_number(`subj`),
         PEprob=y,
         cue=cue) %>% 
  select(rat, PEprob,cue)


stage5DSNSPProb<-subset(stage5DSNSPProb,rat!=3&8)
stage5DSNSPProb<- na.omit(stage5DSNSPProb)

meanstage5DSNSPProb<-stage5DSNSPProb%>%
  group_by(cue) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),PEprob)



stage5DSNSPLat <- stage5DSNSPLat %>%
  mutate(rat = parse_number(`subj`),
         PElat=y,
         cue=cue) %>% 
  select(rat, PElat,cue)


stage5DSNSPLat<-subset(stage5DSNSPLat,rat!=3&8)
stage5DSNSPLat<- na.omit(stage5DSNSPLat)

meanstage5DSNSPLat<-stage5DSNSPLat%>%
  group_by(cue) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),PElat)


