# Acoustic_compostition
##Code associated with thesis chapter on acoustic composition
File description:

sscape48kHz.R 
This script takes in directories of .wav folders, calculates their mean soundscapes for 200Hz bins and outputs a dataframe of mean acoustic composition across the time frame for each frequency bin

matrix_and_pcoa.R
This script takes the dataframes made in sscape48kHz.R, reshapes them so each site is a row and each time/frequency is a column, then runs a pcoa on the outputs.
