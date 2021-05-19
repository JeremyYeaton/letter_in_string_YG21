# Positional cueing, string location variability, and letter-in-string identification
## Yeaton & Grainger, 2021

This repository contains the data processing and analysis scripts for (CITATION). Due to GDPR, the raw data are hosted on OSF: https://osf.io/9678y/

## Data files
- *data_raw_pilot.csv* This data is not reported in the paper, but contains experimental data collected from 49 participants who completed the same task as described in Experiment 1A but with a 200 ms stimulus display time instead of 100 ms.
- *data_raw_exp1a.csv* Raw data from Experiment 1A (100 ms display; ordinal pre cue, or underscore post cue; fixed or variable by 2 characters)
- *data_raw_exp1b.csv* Raw data from Experiment 1B (100 ms display; ordinal pre cue, or underscore post cue; fixed or variable by 4 characters)
- *data_raw_exp2.csv* Raw data from Experiment 2 (100 ms display; ordinal pre cue, or underscore pre cue; fixed or variable by 4 characters)
- *data_raw_exp3.csv* Raw data from Experiment 3 (100 ms display; 13-hash cue instead of 5-hash in Exp 2; fixed or variable by 4 characters)

## Scripts
This repository contains 3 R scripts in the "scripts" directory:
- *A_DataPreprocessing.R* 

## Setup
This analysis pipeline relies on the following R packages:
- tidyverse
- lme4
- lmerTest

It also expects the scripts to be in a "scripts" directory, as well as the data in a "data" dirctory.

## Codebook
### Columns (after preprocessing)
