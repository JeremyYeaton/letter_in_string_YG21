# Data processing for experiments on letter-in-string extraction
# Import and preprocessing
# (C) Jeremy D Yeaton
# January 2021

# Import libraries and raw data -------------------------------------------
library(tidyverse)

# Define %notin% function -- negation of %in% function
'%notin%' <- function(x,y)!('%in%'(x,y))

# # Import pilot/ Experiment 0
# data_raw200 <- read.csv('data/data_raw_pilot.csv') %>%
#   filter(completed == 'yes') %>%
#   mutate(dispLen = '200ms',
#          expNum = 0)

# Import Experiment 1A
data_raw_1a <- read.csv('data/data_raw_exp1a.csv') %>%
  filter(completed == 'yes') %>%
  mutate(dispLen = '100ms',
         expNum = 1)

# Import Experiment 1B
data_raw_1b <- read.csv('data/data_raw_exp1b.csv') %>%
  filter(completed == 'yes') %>%
  select(-copy_of_cueChar) %>%
  mutate(dispLen = '100ms',
         expNum = 2)

# Import Experiment 2
data_raw_2 <- read.csv('data/data_raw_exp2.csv') %>%
  filter(completed == 'yes') %>%
  select(-copy_of_cueChar) %>%
  mutate(dispLen = '100ms',
         expNum = 3)

# Import Experiment 3
data_raw_3 <- read.csv('data/data_raw_exp3.csv') %>%
  filter(completed == 'yes') %>%
  # Remove undefined trial
  filter(Trial_Id != 101) %>%
  select(-copy_of_cueChar) %>%
  mutate(dispLen = '100ms',
         expNum = 4)

# Combine raw data from all experiments
data_raw <- rbind(data_raw_1a,data_raw_1b,data_raw_2,data_raw_3)

# Extract demographic data
demographics <- data_raw %>%
  filter(age != '') %>%
  mutate(age=abs(age)) %>%
  select(exp_subject_id,age,gender,ListOfLanguages,Studies,
         start_time,end_time,expNum)

# Select only relevant columns
brut <- data_raw %>%
  select(exp_subject_id,expNum,Task_Name,Block_Nr,Trial_Nr,Trial_Id,
         Condition_Id,cueChar,StimExpeSelected,subjectInput,xVal) %>%
  # Remove rows that weren't trials
  filter(StimExpeSelected != '')

# Set practice trials aside
practiceTrials <- brut %>%
  filter(Task_Name %notin% c('fixPre','fixPost','varPre','varPost',
                             'fixOrd','varOrd','fixMask','varMask',
                             'fixMask_long','varMask_long'))


# Preprocessing -----------------------------------------------------------

# Set up data frame for analysis
expTrials <- brut %>%
  # Select experimental tasks
  filter(Task_Name %in% c('fixPre','fixPost','varPre','varPost',
                          'fixOrd','varOrd','fixMask','varMask',
                          'fixMask_long','varMask_long')) %>%
  # Specify which letter in the stim string was targeted
  mutate(idx_in_string = case_when(cueChar %in% c('pos1','pos5','pos5b','pos9b') & expNum == 4 ~ 1,
                                   cueChar %in% c('pos2','pos6','pos10') ~ 2,
                                   cueChar %in% c('pos3','pos7','pos11') ~ 3,
                                   cueChar %in% c('pos4','pos8','pos12') ~ 4,
                                   cueChar %in% c('pos9','pos5a','pos9a','pos13') ~ 5,
                                   TRUE ~ as.numeric(Condition_Id))) %>%
  # Extract correct response
  mutate(repCorr = substr(StimExpeSelected,idx_in_string,idx_in_string)) %>%
  # Compare correct response to participant response and judge correctness
  mutate(correct_TF = repCorr == subjectInput | repCorr == toupper(subjectInput)) %>%
  # Specify relevant variables
  # fixPos: whether the string was fixed at center or could also be left or right
  mutate(fixPos = substr(Task_Name,1,3),
         # Whether the cue appeared before (pre) or after (post) the string was presented
         cueTime = ifelse(Task_Name %in% c('fixPost','varPost'),'post','pre'),
         # Was the cue an ordinal digit or an underlined position in a hash mask
         cueType = ifelse(Task_Name %in% c('fixPre','varPre','fixOrd','varOrd'),'digit','mask'),
         # Was the absolute position (abs) on screen shown as the cue, or a relative one (rel)
         absPos = ifelse(expNum == 4,'abs','rel'),
         # Convert participant IDs to factor
         exp_subject_id = as.factor(exp_subject_id),
         # How many characters was a string offset from center
         charOffset = case_when(xVal == 400 ~ 0,
                                xVal %in% c(370,430) ~ 2,
                                xVal %in% c(343,344,456,457) ~ 4),
         # Was the string presented to the left, right, or center
         LR = case_when(xVal == 400 ~ 'Center',
                        xVal < 400 ~ 'Left',
                        xVal > 400 ~ 'Right'),
         # Was the character in the middle of the string or at the ends
         outerInner = ifelse(idx_in_string %in% c(2,3,4),'inner','outer'),
         # Was outer character first or last
         firstLast = case_when(idx_in_string == 1 ~ 'first',
                               idx_in_string == 5 ~ 'last',
                               TRUE ~ 'inner')) %>%
  # Set order of factors 
  mutate(cueTime = factor(cueTime, levels = c('pre','post')),
         outerInner = factor(outerInner,levels=c('outer','inner'))) %>%
  # Remove unnecessary columns
  select(-c(Condition_Id,absPos))

# Write data to file
write_csv(expTrials,'data/exp_trials.csv')

# Clean up workspace
rm(data_raw,brut,practiceTrials,data_raw_1a,data_raw_1b,data_raw_2,data_raw_3)


# Extract and print demographic information -------------------------------

# Number of participants and age information
num_and_age <- demographics %>%
  group_by(expNum) %>%
  summarize(nParticipants = n(),
            age_mean = mean(age),
            age_SD = sd(age))

print(num_and_age)

# Number of participants by gender
gender_breakdown <- demographics %>%
  group_by(expNum, gender) %>%
  summarize(n())

print(gender_breakdown)

rm(num_and_age,gender_breakdown)
