
cat("\f") #clear console


library(readxl)
library(tidyverse)
library(emmeans)
library(lmerTest)
library(plotrix)

setwd("G:/Shared drives/Richard Lab/Papers and Abstracts/Papers in Progress/Alexandra Scott/GADVPFP/Figs/fig3/DFF")

rm(list = ls())
GADVPFPauc1to4 <- read.csv("GADVPFP_dffstage1to4plottab.csv")
GADVPFPaucstage1to4 <- GADVPFPauc1to4 %>%
  mutate(rat = `subject`,
         rat = parse_number(`rat`),
         day= `relday`,
         cue = `cue`,
         led= `led`,
         auc=`aucvalues`,
         sex = `sex`) %>% 
  select(auc, rat, stage, day, cue, led,sex)


GADVPFPauc5 <- read.csv("GADVPFP_dffstage5plottab.csv")
GADVPFPaucstage5 <- GADVPFPauc5 %>%
  mutate(rat = `subject`,
         rat = parse_number(`rat`),
         day= `last10days`,
         cue = `cue`,
         led= `led`,
         auc=`aucvalues`,
         sex = `sex`) %>% 
  select(auc, rat, stage, day, cue, led,sex)
GADVPFPaucstage5 <- na.omit(GADVPFPaucstage5)

test<-subset(GADVPFPaucstage5, cue=='DS') 
mean(test$auc)

testing<-GADVPFPaucstage5%>%
  group_by(day,cue) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),auc)

  
#GADVPFPaucall<-rbind(GADVPFPauc,GADVPFPControlauc)
#GADVPFPaucstage1to4= filter (GADVPFPaucall,stage<5)
#GADVPFPaucstage1to4 <- GADVPFPaucstage1to4 %>%
# select (-cue)
#GADVPFPaucstage5= filter (GADVPFPaucall,stage==5)

# run linear mixed effect models
###
aucstage1to4_model<- lmer(auc ~as.factor(day)*sex+ (1 | rat), data = GADVPFPaucstage1to4)
aucstage1to4_model_nosex<- lmer(auc ~ as.factor(day) + (1 | rat), data = GADVPFPaucstage1to4)
anova(aucstage1to4_model,aucstage1to4_model_nosex)
aucstage1to4_lme<-summary(aucstage1to4_model)
aucstage1to4_lmeanova<- anova(aucstage1to4_model)
capture.output(aucstage1to4_lme, file = "aucstage1to4dff_lme_results.doc")
capture.output(aucstage1to4_lmeanova, file = "aucstage1to4dff_lmeanova_results.doc")


## follow up comparisons 
em <- emmeans(aucstage1to4_model,specs= ~ as.factor(day) *sex)
day1_F <- c(1, rep(0,21))
day1_M<- c(rep(0,11), 1, rep(0,10))
day11_F <- c(rep(0,10), 1, rep(0,11))
day9_M<- c(rep(0,19), 1, rep(0,2))
day9_F <- c(rep(0,8), 1, rep(0,13))
null <-c(rep(0,22))



contrast(em, method = list("day1_F - day1_M" = day1_F - day1_M,
                           "day11_F - day9_M" = day11_F - day9_M,
                           "day9_F - day9_M" = day9_F - day9_M,
                           "day1_F - null" = day1_F - null,
                           "day11_F - null" = day11_F - null,
                           "day9_F - null" = day9_F - null,
                           "day1_M - null" = day1_M - null,
                           "day9_M - null" = day9_M - null))




# run linear mixed effect models
###
aucstage5_model <- lmer(auc ~ day *cue*sex+ (1 | rat), data = GADVPFPaucstage5)
aucstage5_model_nosex <- lmer(auc ~ day*cue+ (1 | rat), data = GADVPFPaucstage5)
anova(aucstage5_model,aucstage5_model_nosex)
aucstage5_lme<-summary(aucstage5_model)
aucstage5_lmeanova<- anova(aucstage5_model)
capture.output(aucstage5_lme, file = "aucstage5dff_lme_results.doc")
capture.output(aucstage5_lmeanova, file = "aucstage5dff_lmeanova_results.doc")

## follow up comparisons 
em1 <- emmeans(aucstage5_model, specs = ~ day *cue*sex )


DS_F <- c(1,0,0,0)
NS_F <- c(0,1,0,0)
DS_M <-c(0,0,1,0)
NS_M <- c(0,0,0,1)
null1 <- c(0,0,0,0)




contrast(em1, method = list("DS_F - NS_F" = DS_F - NS_F,
                            "DS_M - NS_M" = DS_M - NS_M,
                            "DS_F - null" = DS_F - null1,
                            "DS_M - null" = DS_M - null1,
                            "NS_F - null" = NS_F - null1,
                            "NS_M - null" = NS_M - null1,
                            "DS_F - DS_M" = DS_F - DS_M))







library(ggplot2)
# auc with line and area SEM

ggplot(GADVPFPaucstage1to4, aes(x=day, y=auc,color=factor(led))) + 
  stat_summary(fun.data = mean_se,geom = "ribbon")+      
  stat_summary(fun = "mean", geom = "line")+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) +  
  labs(title="AUC Stages 1 -4",x ="Training Day", y = "AUC (0-5 seconds z-scored data) ")+
 scale_x_continuous(limits = c(0,11), breaks = seq(from = 0, to = 11, by = 2))+
  scale_y_continuous(limits = c(0,12),breaks = seq(from = 0, to = 12, by = 2))
ggsave("AUC stages 1to4dff.pdf")

ggplot(GADVPFPaucstage5, aes(x=day, y=auc,color=as.factor(cue))) + 
  stat_summary(fun.data = mean_se,geom = "ribbon")+      
  stat_summary(fun = "mean", geom = "line")+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) + 
  labs(title="AUC Stage 5",x ="Training Day", y = "AUC (0-5 seconds z-scored data) ")+
 scale_x_continuous(limits = c(0,10), breaks = c(2,4,6,8,10))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12))
ggsave("AUC stages 5dff.pdf")

# auc with points and errorbar SEM  
ggplot(GADVPFPaucstage1to4, aes(x=day, y=auc,color=factor(led))) + 
  stat_summary(fun.data = mean_se,geom = "errorbar")+      
  stat_summary(fun = "mean", geom = "point")+
  stat_summary(fun = "mean", geom = "line")+
  geom_point(aes(group=rat))+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) +  
  labs(title="AUC Stages 1 -4",x ="Training Day", y = "AUC (0-5 seconds z-scored data) ") 
#ggsave("AUC stages 1to4_withpointsdff.pdf")

ggplot(GADVPFPaucstage5, aes(x=day, y=auc,linetype=cue)) + 
  stat_summary(fun.data = mean_se,geom = "errorbar")+      
  stat_summary(fun = "mean", geom = "point")+
  stat_summary(fun = "mean", geom = "line")+
  geom_point(aes(group=rat))+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) + 
  labs(title="AUC Stage 5",x ="Training Day", y = "AUC (0-5 seconds z-scored data) ") 
#ggsave("AUC stages 5_withpointsdff.pdf")

