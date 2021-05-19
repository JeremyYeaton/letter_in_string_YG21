# Positional cueing, string location variability, and letter-in-string identification
## Yeaton & Grainger, 2021

This repository contains the data processing and analysis scripts for (CITATION). 

Due to GDPR, the raw data are hosted on OSF: https://osf.io/9678y/

Any questions regarding the data or analysis can be directed to the first author at: jyeaton@uci.edu

## Data files
- **data_raw_pilot.csv** This data is not reported in the paper, but contains experimental data collected from 49 participants who completed the same task as described in Experiment 1A but with a 200 ms stimulus display time instead of 100 ms.
- **data_raw_exp1a.csv** Raw data from Experiment 1A (100 ms display; ordinal pre cue, or underscore post cue; fixed or variable by 2 characters)
- **data_raw_exp1b.csv** Raw data from Experiment 1B (100 ms display; ordinal pre cue, or underscore post cue; fixed or variable by 4 characters)
- **data_raw_exp2.csv** Raw data from Experiment 2 (100 ms display; ordinal pre cue, or underscore pre cue; fixed or variable by 4 characters)
- **data_raw_exp3.csv** Raw data from Experiment 3 (100 ms display; 13-hash cue instead of 5-hash in Exp 2; fixed or variable by 4 characters)

## Setup
This analysis pipeline relies on the following R packages:
- tidyverse
- lme4
- lmerTest

It also expects the scripts to be in a "scripts" directory, as well as the data in a "data" dirctory. The figures script will also expect a "figures" directory.

## Scripts
This repository contains 4 R scripts in the "scripts" directory:
- **A_DataPreprocessing.R** Imports the various data files, removes practice trials (and pilot data), and exports two dataframes: *demographics*, and *expTrials* for which the columns are laid out in the **Codebook** section below. Saves the *expTrials* dataframe in the data directory as *exp_trials.csv*.
- **B_Statistics.R** Contains the code to get the summary statistics and models reported in the paper.
- **C_Figures.R** Contains the code to reproduce the figures in the paper.
- **X1_SerialPositionFigures.R** Contains the code to visualize the Serial Position Functions for each experiment (results separated by position in string instead of aggregated by condition).

## Codebook
### Columns -- *demographics* dataframe
- exp_subject_id: Unique identifier for that participant
- age
- gender: male, female, or non-binary
- ListOfLanguages: manually input list of languages spoken at home in childhood
- Studies: High school or equivalent, Apprenticeship, Bachelor's degree, Master's degree, Doctorate, Other
- start_time: beginning of experimental session in GMT
- end_time: end of experimental session in GMT

### Columns -- *expTrials* dataframe
- exp_subject_id: unique participant identifier
- expNum: experiment number. 1 = Experiment 1A, 2 = Experiment 1B, 3 = Experiment 2, 4 = Experiment 3
- Task_Name: task name for that trial. Redundant info that gets separated into *fixPos*, *cueTime*, and *cueType*
- Block_Nr: order that task was presented to that participant
- Trial_Nr: order that trial was presented within that block
- Trial_Id: unique identifier for letter strings
- cueChar: cued character in the string. For Experiments 1A, 1B, and 2, this is limited to pos1-pos5. For Experiment 3, pos1 corresponds to the leftmost position in the screen and continues right from there. 5a is the 5th character in the string when it is presented on the left. 5b is the 1st character in the string when it is presented in the center. 9a is the 5th character in the string when presented in the center, and 9b is the 1st character when presented on the right. 13 is the leftmost position on the screen.
- StimExpeSelected: target string for that trial
- subjectInput: participant response
- xVal: x-coordinate of the string on the screen. 400 is center, 370 is offset 2 characters to the left 430, is 2 characters to the right. 343/344 are 4-characters to the left, and 456/457 are 4 characters to the right.
- idx_in_string: position in the string. In experiments 1A, 1B, and 2, this is redundant with cueChar.
- repCorr: correct response for that trial -- cued character in the string
- correct_TF: accuracy of response. TRUE for correct, FALSE for incorrect
- fixPos: string location -- fix (fixed) or var (variable)
- cueTime: pre or post string presentation
- cueType: digit (ordinal) or mask (underscore)
- charOffset: number of characters that trial was offset from center (always positive)
- LR: string location on screen -- left, right, or center

Variables not addressed in the paper but useful if interested in the serial position function:
- outerInner: if the cued character was outer (first or last in the string) or inner (positions 2, 3, and 4 in the string)
- firstLast: if the cued character was first or last (or inner)
