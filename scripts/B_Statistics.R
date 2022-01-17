# Data processing for experiments on letter-in-string extraction
# Statistics and Generalized Linear Models
# (C) Jeremy D Yeaton
# February 2021

# Import libraries and data -----------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)

source('scripts/A_DataPreprocessing.R')

# Summary Statistics
expTrials %>%
  group_by(expNum,exp_subject_id) %>%
  summarize(nObs = n(),
            subAcc = mean(correct_TF)) %>%
  ungroup() %>%
  group_by(expNum) %>%
  summarize(observations = sum(nObs),
            mean_Accuracy = mean(subAcc),
            SD_Accuracy = sd(subAcc))

# Experiment 1 ------------------------------------------------------------

## Experiment 1A ####

exp1a.glmer <- expTrials %>%
  filter(expNum == 1) %>%
  glmer(correct_TF ~ cueTime * fixPos + cueChar +
          (cueTime*fixPos|exp_subject_id) * (cueTime*fixPos|Trial_Id),
        data=.,
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp1a.glmer)

## Experiment 1B ####

exp1b.glmer <- expTrials %>%
  filter(expNum == 2) %>%
  glmer(correct_TF ~ cueTime * fixPos + cueChar +
          (cueTime*fixPos|exp_subject_id) * (cueTime|Trial_Id),
        data=.,
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp1b.glmer)


## GLMER of only var data from Exps 1A and 1B ####

exp1ab.glmer <- expTrials %>%
  filter(fixPos == 'var', expNum %in% c(1,2)) %>%
  mutate(expNum = factor(expNum)) %>%
  glmer(correct_TF ~ cueTime * expNum * LR + cueChar +
          (cueTime+LR|exp_subject_id) * (1|Trial_Id),
        data=.,
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp1ab.glmer)

# Experiment 2 ------------------------------------------------------------
# Within Expt 2 (pre ordinal vs. underscore cues)

exp2.glmer <- expTrials %>%
  filter(expNum == 3) %>%
  glmer(correct_TF ~ cueType * fixPos + cueChar + 
          (cueType*fixPos|exp_subject_id) * (1|Trial_Id),
        data=., 
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp2.glmer)

# Between-Expt comparisons (pre and post underscore cue)

maskTime.glmer <- expTrials %>%
  filter(expNum %in% c(2,3),cueType == 'mask') %>%
  mutate(expNum = as.factor(expNum)) %>%
  glmer(correct_TF ~ cueTime * fixPos + cueChar + 
          (cueTime*fixPos|exp_subject_id) * (1|Trial_Id),
        data=., 
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(maskTime.glmer)

# Experiment 3 ------------------------------------------------------------
# Large mask pre-cue in Expt 3 with the same conditions tested with the 
# small mask in Expt 3

maskSize.glmer <- expTrials %>%
  filter(expNum %in% c(3,4), cueType == 'mask') %>%
  mutate(maskSize = ifelse(expNum == 4,'big','small'),
         idx_in_string = as.character(idx_in_string)) %>%
  glmer(correct_TF ~ maskSize * fixPos + idx_in_string + 
          (maskSize+fixPos|exp_subject_id) * (1|Trial_Id),
        data=., 
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(maskSize.glmer)

## Summaries for all glmer models ####
summary(exp1a.glmer)
summary(exp1b.glmer)
summary(exp1ab.glmer)
summary(exp2.glmer)
summary(maskTime.glmer)
summary(maskSize.glmer)

# Accuracy by condition for Exp. 2 and 3
acc_by_sub <- expTrials %>%
  filter(expNum %in% c(3,4), cueType == 'mask', fixPos == 'fix') %>%
  mutate(maskSize = ifelse(expNum == 4,'big','small')) %>%
  group_by(exp_subject_id,maskSize) %>%
  summarize(accuracy = mean(correct_TF))

# Test for difference of means
t.test(x=acc_by_sub$accuracy[acc_by_sub$maskSize=='small'],
       y=acc_by_sub$accuracy[acc_by_sub$maskSize!='small'])

save.image('scripts/letter_in_string_stats.RData')
