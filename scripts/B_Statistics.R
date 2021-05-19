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
  glmer(correct_TF ~ cueTime * fixPos +
          (cueTime*fixPos|exp_subject_id) * (1|Trial_Id) * (1|cueChar),
        data=.,
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp1a.glmer)

## Experiment 1B ####

exp1b.glmer <- expTrials %>%
  filter(expNum == 2) %>%
  glmer(correct_TF ~ cueTime * fixPos +
          (cueTime*fixPos|exp_subject_id) * (1|Trial_Id) * (1|cueChar),
        data=.,
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp1b.glmer)


## GLMER of only var data from exps 1A and 1B ####

exp1ab.glmer <- expTrials %>%
  filter(fixPos == 'var', expNum %in% c(1,2)) %>%
  mutate(expNum = factor(expNum)) %>%
  glmer(correct_TF ~ cueTime * expNum * LR +
          (cueTime*charOffset+LR|exp_subject_id) * (1|Trial_Id) * (1|cueChar),
        data=.,
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp1ab.glmer)

# Experiment 2 ------------------------------------------------------------
# Within Expt 2 (pre ordinal vs. underscore cues)

exp2.glmer <- expTrials %>%
  filter(expNum == 3) %>%
  glmer(correct_TF ~ cueType * fixPos + 
          (cueType*fixPos|exp_subject_id) * (1|Trial_Id) * (1|cueChar),
        data=., 
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(exp2.glmer)

# Between-Expt comparisons (pre and post underscore cue)

maskTime.glmer <- expTrials %>%
  filter(expNum %in% c(2,3),cueType == 'mask') %>%
  mutate(expNum = as.factor(expNum)) %>%
  glmer(correct_TF ~ cueTime * fixPos + 
          (cueTime*fixPos|exp_subject_id) * (1|Trial_Id) * (1|cueChar) * (1|expNum),
        data=., 
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(maskTime.glmer)

# Experiment 3 ------------------------------------------------------------
# Large mask pre-cue in Expt 3 with the same conditions tested with the 
# small mask in Expt 3

maskSize.glmer <- expTrials %>%
  filter(expNum %in% c(3,4), cueType == 'mask') %>%
  mutate(maskSize = ifelse(expNum == 4,'big','small')) %>%
  glmer(correct_TF ~ maskSize * fixPos + 
          (maskSize*fixPos|exp_subject_id) * (1|Trial_Id) * (1|cueChar),
        data=., 
        family = 'binomial',
        control = glmerControl(optimizer = 'bobyqa',optCtrl = list(maxfun = 100000)))
summary(maskSize.io.glmer)