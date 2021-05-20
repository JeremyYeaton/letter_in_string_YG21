# Stimuli
All of the experiments in the paper used the same set of 100 letter strings found in *LIS_strings.csv*.

## Files
- **LIS_strings.csv** Contains the 100 5-letter strings used in the tasks
- **stringPos_fix.csv** Specifies horizontal location for fixed-location trials (all center)
- **stringPos_var.csv** Specifies horizontal location for variable-location trials (evenly split between center, left, and right)
- **LIS_strings_practice.csv** Contains 10 strings that do not appear in the experimental stimuli
- **practice_answers.csv** Cued letters from the practice trials (hard coded so that feedback could be given)
- **practice_answers_min.csv** Same as above, but lower case
- **makeStrings_LIS.py** Python 3 script used to produce all of the above files. Requires NumPy and MatPlotLib.
