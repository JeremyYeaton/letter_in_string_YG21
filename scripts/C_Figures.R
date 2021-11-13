# Data processing for experiments on letter-in-string extraction
# Figures and Visualization
# (C) Jeremy D Yeaton
# February 2021

# Import libraries and data -----------------------------------------------
library(tidyverse)
# library(ggpattern)

# Clear the environment
rm(list=ls()) 

# Read in the data
source('scripts/A_DataPreprocessing.R')

# Assign colors -----------------------------------------------------------

# Colors needed:
color_ord = 'dodgerblue3' # blue
color_und = 'maroon3'     # pink

barSize = 0.5

# Experiment 1A,B ---------------------------------------------------------
## Figure 2 ####
exp1_toplot <- expTrials %>%
  filter(expNum %in% c(1,2)) %>%
  group_by(exp_subject_id,cueTime,fixPos,expNum) %>%
  summarize(acc = mean(correct_TF),.groups='keep') %>%
  ungroup() %>%
  group_by(cueTime,fixPos,expNum) %>%
  summarize(accuracy = mean(acc),
            se = sd(acc)/n()^.5,.groups='keep') %>%
  ungroup() %>%
  mutate(cueTime = case_when(cueTime == 'pre' ~ 'Ordinal',
                             cueTime == 'post' ~ 'Underscore'),
         fixPos = case_when(fixPos == 'fix' ~ 'Fixed location',
                            fixPos == 'var' ~ 'Variable location')) %>%
  mutate(cueTime = factor(cueTime, levels = c('Ordinal','Underscore')),
         expNum = factor(expNum))

exp1.plot <- exp1_toplot %>%
  mutate(expNum = ifelse(expNum == 1, 'Experiment 1A','Experiment 1B')) %>%
  ggplot(aes(fill=cueTime,y=accuracy,x=fixPos)) +
  geom_bar(stat='identity',position='dodge',width = 0.75) +
  # geom_bar_pattern(aes(pattern=cueTime),stat='identity',position='dodge',width=0.75,
  #                  pattern_fill='black',pattern_colour='black',pattern_spacing=0.025,pattern_size=0) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),
                size=barSize,show.legend = FALSE,color='black',position=position_dodge(0.75)) +
  facet_grid(~expNum) +
  coord_cartesian(ylim=c(0,1)) +
  # scale_fill_manual(values=c(color_ord,color_und)) +
  # scale_pattern_manual(values=c(1,2)) +
  labs(y='Accuracy',x='String location',fill='Cue type',pattern='Cue type',linetype='Experiment') +
  theme_bw() +
  theme(legend.position = 'bottom')
exp1.plot
exp1.plot %>%
  # ggsave(filename = 'figures/figure2.tiff',plot=.,width=16,height=9,units='cm',dpi=300)
  ggsave(filename = 'figures/figure2.jpeg',plot=.,width=16,height=9,units='cm',dpi=300)

## Figure 3 ####
# Variable condition split by left/middle/right
varTrials_toplot <- expTrials %>%
  filter(expNum %in% c(1,2), fixPos == 'var') %>%
  group_by(exp_subject_id,cueTime,LR,expNum) %>%
  summarize(acc = mean(correct_TF),.groups='keep') %>%
  ungroup() %>%
  group_by(cueTime,LR,expNum) %>%
  summarize(accuracy = mean(acc),
            se = sd(acc)/n()^.5,.groups='keep') %>%
  ungroup() %>%
  mutate(cueTime = case_when(cueTime == 'pre' ~ 'Ordinal',
                             cueTime == 'post' ~ 'Underscore'),
         LR = factor(LR,levels=c('Left','Center','Right'))) %>%
  mutate(cueTime = factor(cueTime, levels = c('Ordinal','Underscore')),
         expNum = factor(expNum))

varTrials.plot <- varTrials_toplot %>%
  mutate(expNum = ifelse(expNum == 1, 'Experiment 1A','Experiment 1B')) %>%
  ggplot(aes(fill=cueTime,y=accuracy,x=LR)) +
  geom_bar(stat='identity',position='dodge',width = 0.75) +
  # geom_bar_pattern(aes(pattern=cueTime),stat='identity',position='dodge',width=0.75,
  #                  pattern_fill='black',pattern_colour='black',pattern_spacing=0.025,pattern_size=0) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),
                size=barSize,show.legend = FALSE,color='black',position=position_dodge(0.75)) +
  facet_grid(~expNum) +
  coord_cartesian(ylim=c(0,1)) +
  # scale_fill_manual(values=c(color_ord,color_und)) +
  labs(y='Accuracy',x='Location on screen',fill='Cue type',pattern='Cue type',linetype='Experiment') +
  theme_bw() +
  theme(legend.position = 'bottom')
varTrials.plot
varTrials.plot %>%
  # ggsave(filename = 'figures/figure3.tiff',plot=.,width=16,height=9,units='cm',dpi=300)
  ggsave(filename = 'figures/figure3.jpeg',plot=.,width=16,height=9,units='cm',dpi=300)

# Experiment 2 ------------------------------------------------------------
## Figure 5 ####
exp2_toplot <- expTrials %>%
  filter(expNum %in% c(2,3)) %>%
  group_by(exp_subject_id,cueType,fixPos,expNum) %>%
  summarize(acc = mean(correct_TF),.groups='keep') %>%
  ungroup() %>%
  group_by(cueType,fixPos,expNum) %>%
  summarize(accuracy = mean(acc),
            se = sd(acc)/n()^.5,.groups='keep') %>%
  ungroup() %>%
  mutate(cueType = case_when(cueType == 'mask' ~ 'Underscore',
                             cueType == 'digit' ~ 'Ordinal'),
         fixPos = case_when(fixPos == 'fix' ~ 'Fixed',
                            fixPos == 'var' ~ 'Variable'),
         expNum = case_when(expNum == 2 ~ 'Experiment 1B',
                            expNum == 3 ~ 'Experiment 2'))

exp2.plot <- exp2_toplot %>%
  ggplot(aes(fill=cueType,y=accuracy,x=fixPos)) +
  geom_bar(stat='identity',position='dodge',width = 0.75) +
  # geom_bar_pattern(aes(pattern=cueType),stat='identity',position='dodge',width=0.75,
  #                  pattern_fill='black',pattern_colour='black',pattern_spacing=0.025,pattern_size=0) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),
                size=barSize,show.legend = FALSE,color='black',position=position_dodge(0.75)) +
  facet_grid(~expNum) +
  coord_cartesian(ylim=c(0,1)) +
  # scale_fill_manual(values=c(color_ord,color_und)) +
  labs(y='Accuracy',x='String location',fill='Cue type',pattern='Cue type',linetype='Experiment') +
  theme_bw() +
  theme(legend.position = 'bottom')
exp2.plot
exp2.plot %>%
  # ggsave(filename = 'figures/figure5.tiff',plot=.,width=16,height=9,units='cm',dpi=300)
  ggsave(filename = 'figures/figure5.jpeg',plot=.,width=16,height=9,units='cm',dpi=300)


## Figure 6 ####
exp2_var.toplot <- expTrials %>%
  filter(expNum %in% c(3)) %>%
  filter(fixPos == 'var') %>%
  group_by(exp_subject_id,cueType,LR,expNum) %>%
  summarize(acc = mean(correct_TF),.groups='keep') %>%
  ungroup() %>%
  group_by(cueType,LR,expNum) %>%
  summarize(accuracy = mean(acc),
            se = sd(acc)/n()^.5,.groups='keep') %>%
  ungroup() %>%
  mutate(cueType = case_when(cueType == 'mask' ~ 'Underscore',
                             cueType == 'digit' ~ 'Ordinal'),
         LR = factor(LR,levels= c('Left','Center','Right')),
         expNum = case_when(expNum == 2 ~ 'Experiment 1B',
                            expNum == 3 ~ 'Experiment 2'))

exp2_var.plot <- exp2_var.toplot %>%
  ggplot(aes(fill=cueType,y=accuracy,x=LR)) +
  geom_bar(stat='identity',position='dodge',width = 0.75) +
  # geom_bar_pattern(aes(pattern=cueType),stat='identity',position='dodge',width=0.75,
  #                  pattern_fill='black',pattern_colour='black',pattern_spacing=0.025,pattern_size=0) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),
                size=barSize,show.legend = FALSE,color='black',position=position_dodge(0.75)) +
  coord_cartesian(ylim=c(0,1)) +
  # scale_fill_manual(values=c(color_ord,color_und)) +
  labs(y='Accuracy',x='Location on screen',fill='Cue type',pattern='Cue type',linetype='Experiment') +
  theme_bw() +
  theme(legend.position = 'bottom')
exp2_var.plot
exp2_var.plot %>%
  # ggsave(filename = 'figures/figure6.tiff',plot=.,width=16,height=9,units='cm',dpi=300)
  ggsave(filename = 'figures/figure6.jpeg',plot=.,width=16,height=9,units='cm',dpi=300)

# Experiment 3 ------------------------------------------------------------
## Figure 7 ####
exp3_toplot <- expTrials %>%
  filter(expNum %in% c(3,4),cueType == 'mask') %>%
  group_by(exp_subject_id,cueType,fixPos,expNum) %>%
  summarize(acc=mean(correct_TF),.groups='keep') %>%
  ungroup() %>%
  group_by(cueType,fixPos,expNum) %>%
  summarize(se = sd(acc)/n()^.5,
            accuracy = mean(acc),.groups='keep') %>%
  ungroup() %>%
  mutate(fixPos = case_when(fixPos == 'fix' ~ 'Fixed',
                            fixPos == 'var' ~ 'Variable'),
         expNum = case_when(expNum == 3 ~ 'Experiment 2',
                            expNum == 4 ~ 'Experiment 3'))

exp3.plot <- exp3_toplot %>%
  ggplot(aes(y=accuracy,x=fixPos)) +
  geom_bar(stat='identity',position='dodge',width = 0.75, fill = '#00BFC4') + #,fill=color_und) +
  # geom_bar_pattern(aes(pattern=cueType),stat='identity',position='dodge',width=0.75,fill=color_und,
  #                  pattern_fill='black',pattern_colour='black',pattern_spacing=0.025,pattern='circle',pattern_size=0) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),
                size=barSize,show.legend = FALSE,color='black',position=position_dodge(0.75)) +
  facet_grid(~expNum) +
  coord_cartesian(ylim=c(0,1)) +
  labs(y='Accuracy',fill='String location',x='String location',linetype='Experiment') +
  theme_bw() +
  theme(legend.position = 'none')
exp3.plot
exp3.plot %>%
  # ggsave(filename = 'figures/figure7.tiff',plot=.,width=16,height=9,units='cm',dpi=300)
  ggsave(filename = 'figures/figure7.jpeg',plot=.,width=16,height=9,units='cm',dpi=300)

## Figure 8 ####
# Variable condition split by left/middle/right
varTrials3_toplot <- expTrials %>%
  filter(expNum %in% c(3,4), fixPos == 'var') %>%
  group_by(exp_subject_id,cueType,LR,expNum) %>%
  summarize(acc = mean(correct_TF),.groups='keep') %>%
  ungroup() %>%
  group_by(cueType,LR,expNum) %>%
  summarize(accuracy = mean(acc),
            se = sd(acc)/n()^.5,.groups='keep') %>%
  ungroup() %>%
  mutate(cueType = case_when(cueType == 'digit' ~ 'Ordinal',
                             cueType == 'mask' ~ 'Underscore'),
         LR = factor(LR,levels=c('Left','Center','Right'))) %>%
  mutate(cueType = factor(cueType, levels = c('Ordinal','Underscore')),
         expNum = factor(expNum))

varTrials3.plot <- varTrials3_toplot %>%
  mutate(expNum = ifelse(expNum == 3, 'Experiment 2','Experiment 3')) %>%
  ggplot(aes(fill=cueType,y=accuracy,x=LR)) +
  geom_bar(stat='identity',position='dodge',width = 0.75) +
  # geom_bar_pattern(aes(pattern=cueTime),stat='identity',position='dodge',width=0.75,
  #                  pattern_fill='black',pattern_colour='black',pattern_spacing=0.025,pattern_size=0) +
  geom_errorbar(aes(ymin=accuracy-se,ymax=accuracy+se,width= .25),
                size=barSize,show.legend = FALSE,color='black',position=position_dodge(0.75)) +
  facet_grid(~expNum) +
  coord_cartesian(ylim=c(0,1)) +
  # scale_fill_manual(values=c(color_ord,color_und)) +
  labs(y='Accuracy',x='Location on screen',fill='Cue type',pattern='Cue type',linetype='Experiment') +
  theme_bw() +
  theme(legend.position = 'bottom')
varTrials3.plot
varTrials3.plot %>%
  # ggsave(filename = 'figures/figure8.tiff',plot=.,width=16,height=9,units='cm',dpi=300)
  ggsave(filename = 'figures/figure8.jpeg',plot=.,width=16,height=9,units='cm',dpi=300)
