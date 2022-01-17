# Data processing for experiments on letter-in-string extraction
# Statistics and Figures for Error types
# (C) Jeremy D Yeaton
# August 2021

# Import libraries and data -----------------------------------------------

library(tidyverse)
library(stringr)

source('scripts/A_DataPreprocessing.R')

# Analysis of error types -------------------------------------------------
errorTypes <- expTrials %>%
  filter(correct_TF == FALSE) %>%
  mutate(rep_in_string = str_detect(StimExpeSelected,fixed(subjectInput,ignore_case = T)))

err_summary.tbl <- errorTypes %>%
  group_by(expNum) %>%
  summarize(errs_in_exp = n()) %>%
  merge(errorTypes) %>%
  group_by(rep_in_string,expNum,errs_in_exp) %>%
  summarize(n_errs = n()) %>%
  mutate(prop_errs = n_errs/errs_in_exp)

errorDist <- errorTypes %>%
  filter(rep_in_string == TRUE) %>%
  mutate(pos_of_err = str_locate(StimExpeSelected,toupper(subjectInput))[,1]) %>%
  mutate(errDist = pos_of_err-idx_in_string) %>%
  mutate(absDist = abs(errDist)) %>%
  mutate(expName = case_when(expNum == 1 ~ '1A',
                             expNum == 2 ~ '1B',
                             expNum == 3 ~ '2',
                             expNum == 4 ~ '3'))

byExp_err.plot <- errorDist %>%
  ggplot(data=.,aes(x=errDist)) +
  geom_bar() +
  facet_grid(expName~idx_in_string) +
  labs(y="Number of errors",x='Distance from cued letter') 
byExp_err.plot

exp1A_err.plot <- errorDist %>%
  filter(expNum == 1) %>%
  mutate(LR = factor(LR, levels = c('Left','Center','Right')),
         cueTime = if_else(cueTime == 'pre', 'Pre','Post'),
         fixPos = if_else(fixPos == 'fix', 'Fixed','Variable')) %>%
  ggplot(aes(x=errDist,fill = cueTime)) +
  geom_bar(position='dodge') +
  facet_grid(fixPos~LR) +
  scale_x_continuous(breaks=c(-4,-3,-2,-1,0,1,2,3,4)) +
  labs(x= 'Response distance from cued letter',
       y = 'Number of errors',
       fill = 'Cue time')
exp1A_err.plot

exp1B_err.plot <- errorDist %>%
  filter(expNum == 2) %>%
  mutate(LR = factor(LR, levels = c('Left','Center','Right')),
         cueTime = if_else(cueTime == 'pre', 'Pre','Post'),
         fixPos = if_else(fixPos == 'fix', 'Fixed','Variable')) %>%
  ggplot(aes(x=errDist,fill = cueTime)) +
  geom_bar(position='dodge') +
  facet_grid(fixPos~LR) +
  scale_x_continuous(breaks=c(-4,-3,-2,-1,0,1,2,3,4)) +
  labs(x= 'Response distance from cued letter',
       y = 'Number of errors',
       fill = 'Cue time')
exp1B_err.plot

exp2_err.plot <- errorDist %>%
  filter(expNum == 3) %>%
  mutate(LR = factor(LR, levels = c('Left','Center','Right')),
         cueType = if_else(cueType == 'digit', 'Ordinal','Underscore'),
         fixPos = if_else(fixPos == 'fix', 'Fixed','Variable')) %>%
  ggplot(aes(x=errDist,fill=cueType)) +
  geom_bar(position='dodge') +
  facet_grid(fixPos~LR) +
  scale_x_continuous(breaks=c(-4,-3,-2,-1,0,1,2,3,4)) +
  labs(x= 'Response distance from cued letter',
       y = 'Number of errors',
       fill = 'Cue type')
exp2_err.plot

exp3_err.plot <- errorDist %>%
  filter(expNum == 4) %>%
  mutate(LR = factor(LR, levels = c('Left','Center','Right')),
         fixPos = if_else(fixPos == 'fix', 'Fixed','Variable')) %>%
  ggplot(aes(x=errDist)) +
  geom_bar(position='dodge') +
  facet_grid(fixPos~LR)  +
  scale_x_continuous(breaks=c(-4,-3,-2,-1,0,1,2,3,4)) +
  labs(x= 'Response distance from cued letter',
       y = 'Number of errors')
exp3_err.plot
