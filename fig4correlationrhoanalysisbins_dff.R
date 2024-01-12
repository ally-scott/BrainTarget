cat("\f") #clear console
library(lmerTest)

library(tidyverse)
library(readxl)
library(emmeans)

library(dplyr)
library(readr)

setwd("G:/Shared drives/Richard Lab/Papers and Abstracts/Papers in Progress/Alexandra Scott/GADVPFP/Figs/fig4/DFF")


rm(list = ls())
latcorrbin <- read.csv("PElatencycorrelationtableforanalysis_DFF_bins.csv")
latcorrbin <- latcorrbin  %>%
  mutate(rat = `rat`,
         rat = parse_number(`rat`),
         rho = `rho_bins`,
         type=`event`) %>% 
  select(type, rat, rho, bin)


ratinfo<- read.csv("ratinfo.csv")
names(ratinfo)[1]<- 'rat'
ratinfo  <-  ratinfo %>%
  mutate(rat = `rat`,
         sex = `sex`) %>% 
  select( rat,sex)


bins<- merge(latcorrbin, ratinfo, all = TRUE, by = c('rat'))
bins<-filter(bins, rat != 3 )
bins<-filter(bins, bin < 1.5)

#exclude rats


# bin_seq <- unique(bins$bin)
# summary(bins)
# ttest_fun <- function(t) {
#   browser()
#   df <- bins %>%
#     filter(bin == t)
#   test <- t.test(df$binavg_b)
#   p_value <- test$p.value
#   return(p_value)
# }
# 
# try <- lapply(bin_seq, FUN = ttest_fun(t = x))

# run linear mixed effect models
###
bin_model <- lmer(rho~ as.factor(bin)*type+ (1 | rat), data = bins)

bin_lme<-summary(bin_model)
capture.output(bin_lme, file = "binrho_lme_dff.doc")


bin_lmeanova<- anova(bin_model)
capture.output(bin_lmeanova, file = "binrho_lmeanova_dff.doc")

library(broom)

test<- bins %>%
  group_by(bin) %>%
  do( tidy(t.test(data=., rho~as.factor(type),paired=TRUE, p.adjust.method = "bonferroni")))

write.csv(test,"G:\\Shared drives\\Richard Lab\\Papers and Abstracts\\Papers in Progress\\Alexandra Scott\\GADVPFP\\Figs\\fig4\\DFF\\bin_rho_ttest_dff.csv")

#capture.output(bin_lme, file = "binkernel_lme_results.doc")
#capture.output(bin_lmeanova, file = "binkernel_lmeanova_results.doc")




# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
## need  to write a for loop for each bin? 
#stat.test <- bins %>%
 # group_by(bins) %>%
 # t_test(binavg_b ) %>%
 # adjust_pvalue(method = "BH") %>%
 # add_significance()
#stat.test







library(ggplot2)


ggplot(bins, aes(x=bin, y=bins)) + 
  stat_summary(fun.data = mean_se,geom = "ribbon")+      
  stat_summary(fun = "mean", geom = "line")+
  
  facet_grid(vars(sex), vars(type)) + 
  
  #stat_summary(fun.data = mean_se,geom = "ribbon")+      
  #stat_summary(fun = "mean", geom = "line")+
  
  
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  
  labs(title="time bins criteria day kernels",x ="event type", y = "b coeff from kernel ")  
ggsave("ribbon_bin criteria day kernels_dff.pdf")



ggplot(bins, aes(x=bin, y=binavg_b,color=led)) + 
  stat_summary(fun.data = mean_se,geom = "ribbon")+      
  stat_summary(fun = "mean", geom = "line")+
  
  facet_grid(vars(type)) + 
  
  #stat_summary(fun.data = mean_se,geom = "ribbon")+      
  #stat_summary(fun = "mean", geom = "line")+
  
  
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  
  labs(title="time bins criteria day kernels",x ="event type", y = "b coeff from kernel ")  
ggsave("ribbon_bin criteria day kernels_nosex_dff.pdf")
