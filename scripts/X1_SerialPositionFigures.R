# Data processing for experiments on letter-in-string extraction
# Supplementary Figures and Visualization: Serial Position Functions
# (C) Jeremy D Yeaton
# February 2021

# Import libraries and data -----------------------------------------------
library(tidyverse)

# Read in the data
source('scripts/A_DataPreprocessing.R')


# Assign colors -----------------------------------------------------------

# Colors needed:
color_exp1a = 'chartreuse3' # green
color_exp1b = 'chocolate2'  # orange
color_exp2 = 'dodgerblue3'  # blue
color_exp3 = 'maroon3'      # pink

theme_set(theme_bw())

barSize = 0.75

# Experiment 1 ------------------------------------------------------------

# Both on same axes with both fix and variable condition
exp1_toplot <- expTrials %>%
  filter(expNum %in% c(1,2)) %>%
  group_by(Task_Name,expNum) %>%
  summarize(overall_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(1,2))) %>% 
  group_by(exp_subject_id,overall_mean,Task_Name,expNum) %>%
  summarize(sub_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(1,2)) %>%
          group_by(exp_subject_id,cueTime,fixPos,cueChar,Task_Name,expNum) %>%
          summarize(cond_mean = mean(correct_TF),.groups='keep')) %>%
  mutate(acc = cond_mean - sub_mean + overall_mean) %>%
  group_by(cueChar,cueTime,fixPos,Task_Name,expNum) %>%
  summarize(se = sd(acc)/n()^.5,
            accuracy = mean(acc),.groups='keep') %>%
  ungroup() %>%
  mutate(cueTime = case_when(cueTime == 'pre' ~ 'Pre-cue',
                             cueTime == 'post' ~ 'Post-cue'),
         fixPos = case_when(fixPos == 'fix' ~ 'Fixed string location',
                            fixPos == 'var' ~ 'Variable string location')) %>%
  mutate(cueTime = factor(cueTime, levels = c('Pre-cue','Post-cue')),
         expNum = factor(expNum),
         task = paste(Task_Name, expNum, sep='_'),
         cueChar = as.numeric(substr(cueChar,4,4)))


exp1SPF.plot <- exp1_toplot %>%
  mutate(expNum = ifelse(expNum == 1, '1A','1B')) %>%
  ggplot(aes(x=cueChar,y=accuracy,group=task,color=expNum)) +
  geom_path(aes(linetype=expNum)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),size=barSize,show.legend = FALSE) +
  facet_grid(cueTime~fixPos) +
  coord_cartesian(ylim=c(0,1)) +
  scale_linetype_manual(values = c(1,2)) +
  scale_color_manual(values=c(color_exp1a,color_exp1b)) +
  labs(y='Accuracy',x='Position in string',color='Experiment',linetype='Experiment')
exp1SPF.plot

# Variable condition split by left/middle/right
varTrials_toplot <- expTrials %>%
  filter(expNum %in% c(1,2), fixPos == 'var') %>%
  group_by(Task_Name,expNum) %>%
  summarize(overall_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(1,2), fixPos == 'var')) %>% 
  group_by(exp_subject_id,overall_mean,Task_Name,expNum) %>%
  summarize(sub_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(1,2), fixPos == 'var') %>%
          group_by(exp_subject_id,cueTime,LR,cueChar,Task_Name,expNum) %>%
          summarize(cond_mean = mean(correct_TF),.groups='keep')) %>%
  mutate(acc = cond_mean - sub_mean + overall_mean) %>%
  group_by(cueChar,cueTime,LR,Task_Name,expNum) %>%
  summarize(se = sd(acc)/n()^.5,
            accuracy = mean(acc),.groups='keep') %>%
  ungroup() %>%
  mutate(cueTime = case_when(cueTime == 'pre' ~ 'Pre-cue',
                             cueTime == 'post' ~ 'Post-cue'),
         LR = factor(LR,levels=c('Left','Center','Right')),
         LRexp = paste(LR,expNum)) %>%
  mutate(cueTime = factor(cueTime, levels = c('Pre-cue','Post-cue')),
         expNum = factor(expNum),
         task = paste(Task_Name, expNum, sep='_'),
         cueChar = as.numeric(substr(cueChar,4,4))) 

varTrialsSPF.plot <- varTrials_toplot %>%
  mutate(expNum = ifelse(expNum == 1, '1A','1B')) %>%
  ggplot(aes(x=cueChar,y=accuracy,color=expNum,group=LRexp)) +
  geom_path(aes(linetype=expNum)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),size=barSize,show.legend=FALSE) +
  facet_grid(cueTime~LR) +
  coord_cartesian(ylim=c(0,1)) +
  labs(y='Accuracy',x='Position in string',color = 'Experiment',linetype='Experiment') +
  scale_color_manual(values=c(color_exp1a,color_exp1b)) +
  scale_linetype_manual(values=c(1,2))
varTrialsSPF.plot

# Experiment 2 ------------------------------------------------------------

exp2_toplot <- expTrials %>%
  filter(expNum %in% c(2,3)) %>%
  group_by(Task_Name,expNum) %>%
  summarize(overall_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(2,3))) %>%
  group_by(exp_subject_id,overall_mean,Task_Name,expNum) %>%
  summarize(sub_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(2,3)) %>%
  group_by(exp_subject_id,cueType,fixPos,cueChar,Task_Name,expNum) %>%
  summarize(cond_mean = mean(correct_TF),.groups='keep')) %>%
  mutate(acc = cond_mean - sub_mean + overall_mean) %>%
  group_by(cueChar,cueType,fixPos,Task_Name,expNum) %>%
  summarize(se = sd(acc)/n()^.5,
            accuracy = mean(acc),.groups='keep') %>%
  ungroup() %>%
  mutate(cueType = case_when(cueType == 'mask' ~ 'Underscore cue',
                             cueType == 'digit' ~ 'Ordinal cue'),
         fixPos = case_when(fixPos == 'fix' ~ 'Fixed string location',
                            fixPos == 'var' ~ 'Variable string location'),
         cueChar = as.numeric(substr(cueChar,4,4)),
         expNum = as.character(expNum))

exp2SPF.plot <- exp2_toplot %>%
  mutate(expNum = ifelse(expNum == 2, '1B','2')) %>%
  ggplot(aes(x=cueChar,y=accuracy,color=expNum,group=Task_Name)) +
  geom_path(aes(linetype=expNum)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),size=barSize,show.legend = FALSE) +
  facet_grid(cueType~fixPos) +
  coord_cartesian(ylim=c(0,1)) +
  labs(y='Accuracy',x='Position in string',color = 'Experiment',linetype='Experiment') +
  scale_color_manual(values=c(color_exp1b,color_exp2)) +
  theme(line = element_line(linetype=c(1,2))) +
  scale_linetype_manual(values=c(2,3))
exp2SPF.plot

# Experiment 3 ------------------------------------------------------------

exp3_toplot <- expTrials %>%
  filter(expNum %in% c(3,4), cueType == 'mask') %>%
  group_by(Task_Name,expNum) %>%
  summarize(overall_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(3,4), cueType == 'mask')) %>%
  group_by(exp_subject_id,overall_mean,Task_Name,expNum) %>%
  summarize(sub_mean = mean(correct_TF),.groups='keep') %>%
  merge(expTrials %>%
          filter(expNum %in% c(3,4), cueType == 'mask') %>%
          mutate(realPos = case_when(LR == 'Left' ~ idx_in_string,
                                     LR == 'Center' ~ idx_in_string + 4,
                                     LR == 'Right' ~ idx_in_string + 8)) %>%
          group_by(exp_subject_id,cueType,fixPos,realPos,Task_Name,LR,expNum) %>%
          summarize(cond_mean = mean(correct_TF),.groups='keep')) %>%
  mutate(acc = cond_mean - sub_mean + overall_mean) %>%
  group_by(realPos,cueType,fixPos,Task_Name,expNum,LR) %>%
  summarize(se = sd(acc)/n()^.5,
            accuracy = mean(acc),.groups='keep') %>%
  ungroup() %>%
  mutate(cueType = case_when(cueType == 'mask' ~ 'Mask cue',
                             cueType == 'digit' ~ 'Underscore cue'),
         fixPos = case_when(fixPos == 'fix' ~ 'Fixed string location',
                            fixPos == 'var' ~ 'Variable string location'),
         cueChar = realPos,
         expNum = as.character(expNum),
         task = paste(Task_Name,LR,sep='_'))

exp3SPF.plot <- exp3_toplot %>%
  mutate(expNum = ifelse(expNum == 3, '2','3')) %>%
  ggplot(aes(x=cueChar,y=accuracy,color=expNum,group=task)) +
  geom_path(aes(linetype=expNum)) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),size=barSize,show.legend = FALSE) +
  facet_wrap(~fixPos,nrow=2) +
  coord_cartesian(ylim=c(0,1)) +
  labs(y='Accuracy',x='Location on screen',color = 'Experiment',linetype='Experiment') +
  scale_color_manual(values=c(color_exp2,color_exp3)) +
  scale_linetype_manual(values = c(3,4))
exp3SPF.plot

exp1SPF.plot
varTrialsSPF.plot
exp2SPF.plot
exp3SPF.plot
