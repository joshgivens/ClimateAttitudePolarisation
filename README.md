# German Climate Attitiudes
All Code and data for the Paper on German attitudes towards Climate change

Below we detail each of the folders and their contents.

# Code
The code folder contains all code for both data manipulation and model fitting in the following subfolders.

## Analysis
Contains all code for model fitting. 
- `correlations.R`: Produces simple corrleations for each of our explanatory variables
- `party_models.R`: Fits panel models with random effects for each individual to explore the effect of party affiliation on climate change support over time.
- `political_leaning_models.R`: Fits panel models with random effects for each individual to explore the effect of political leaning (both economic and social) on climate change support over time.
- `riclpm.R` : Contains code to fit the Random Intercept Cross Lagged Panel Model (RICLPM) to the data. This model is used to explore the relationship affiliation with AfD and climate change attitudes over time.

## Data_summaries
Internal summaries of the data for exploratory/diagnostic purposes.

## Tables_and_Figures
Code to produce all of the tables and figures for the paper.


## Data Manipulation
The script `data_read_and_tidy.R` reads in and merges the raw data for each wave for a few key questions this is then written out to `full_raw.rds` and `full_raw.csv`.

`data_manipulation.R` takes this merged data (from `full_raw.rds`) tidies to produce data for model fitting. This data is then written out to `full_tidied.rds` and `full_tidied.csv`

# Data
The data folder contains all manipulated data. 

## `full_raw`
The `.rds` and `.csv` versions of these files contain the raw data from each wave merged vertically including the variable mentioned above. The wave in which the question was asked is stored in the variable `Cohort`.

## `full_tidied`
The `.rds` and `.csv` versions of these files contain the `full_raw` data with the new more readable variables.

These variables are:
* `lfdn`: The unique identifier for each individual in the study.
* `gender`: An individuals gender ('M' or 'F')
* `Age_2020`: Individuals Age as of 2022 (assumes they were born of the 1st January on the year of Birth.)
      All people born before 1955 are registered as being born in 1955 so anyone who has age 67 should be interpreted as 67+ 
* `left_vs_right`: Which way do they lean politically. High values indicate right leaning.
* `polit_int`: Political Interest. High values indicate strong political interest.
* `econ_vs_clim`: Should Climate take Precedence over Economic Growth. High values indicate that Climate Change should take precedence.
* `tax_and_soc_scale`: Should Social service be increased at cost of more tax. High values indicate that they want more social services.
* `immigration_difficulty_scale`: Should immigration be more difficult. High values indicate more difficult immigration
* `econ_and_clim_imp`: How important are Climate Change and Economic Growth. High values indicate high importance.
* `party`: What political party do you favour?  

Each of the questionnaire variables 2 additional associated variables:


* `p_<variable>`: e.g. `p_cve` A string indicating on which waves was the question answered. Waves are given in order with a 1 indicating that the question was answered and a 0 indicating otherwise. For example if an individual answered `cve` in waves 1,3,5-8,10,13, their entry would be `1-0-1-0-1-1-1-1-0-0-1-0-0-1-0-0`.

* `n_<variables>`: e.g. `n_cve` The number of waves the question was answered.

### Some useful patterns in variables

* Variables ending in `_imp` give importance with a scale going from -2=Not important at all - 2=Very important. 

* Any variable of the form `<a>_vs_<b>` has higher values preferring `<b>` and 0 being no preference.
* Any variable ending `<a>_scale` are questions asking how much of `<a>` they want with -3=As little as possible and 3=As much as possible. 

# Modeldata
Due to the time that the fitting of some models took, the output the fitted models foreach of our analyses were saved into this folder.

# Rawdata
The full documentation for the rawdata can be found here: [https://search.gesis.org/research_data/ZA6838].

Below are some key columns as well as information about them

* `lfdn` Participant Id
* `kpx_2290` Year of Birth
* `kpx_2290` Gender: 1=M, 2=F.
* `ostwest` Living in East or West Germany: 1=West, 0=East.
* `kpx_1500` Political Leaning: 1-11, 1=Far Left  2=Far Right. Asked in 1-4,6,7,8,10-16,19,22,23.
* `kpx_010` Political Interest: 1-5, 1=Very Interested, 5=Not at all Interested. Asked in 1-23.
* `kpx_1290` Should Climate take Precedence over Economic Growth: 1-7, 1=Climate should take precedence 7=Economic growth should take precedence. Asked in 1,2,4,7,8,10-20,22,23.
* `kpx 1090` Should Social service be increased at cost of more tax. 1=lower taxes. 7=more social services. Asked in   1-4,a1,6-8,10-17,19,20,22,23.
* `kpx 1130` Should immigration be harder. 1=immigration should be easier, 7=immigration should be harder. Asked in 1-4,a1,6-8,10-17,19-23.
* `kpx_1300` How important are Climate Change and Economic Growth: 1-5, 1=Very Important, 5=Not Important at all. Asked in 2,4,7,8,10-15.
* `kpx_2090a` What political party do you identify with. Asked in 1-23

# Figures
Contains all the figures summarising our data and analyses.

# Tables
Contains all the tables summarising our data and analyses.



