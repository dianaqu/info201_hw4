# a4-data-wrangling

################################### Set up ###################################

# Install (if not installed) + load dplyr package 
#install.packages('dplyr')
library(dplyr)

# Set your working directory to the appropriate project folder
setwd("~/Dropbox/INFO 201/hw4")

# Read in `any_drinking.csv` data using a relative path
any_drinking <- read.csv('data/any_drinking.csv', stringsAsFactors = FALSE)
# Read in `binge.drinking.csv` data using a relative path
binge_drinking <- read.csv('data/binge_drinking.csv', stringsAsFactors = FALSE)
# Create a directory (using R) called "output" in your project directory
dir.create("output")
################################### Any drinking in 2012 ###################################

# For this first section, let's just work with the columns `state`, `location`, and the data from 2012
# (from the *any drinking* dataset)
# Create a data.frame that has the `state` and `location` columns, and all columns from 2012
location.2012 <- select(any_drinking, state, location, contains("_2012"))

# Using the 2012 data, create a column that has the difference in male and female drinking patterns
location.2012 <- mutate(location.2012, difference = males_2012 - females_2012)

# Write your 2012 data to a .csv file in your `output/` directory with an expressive filename
write.csv(location.2012, 'output/location.2012.csv', row.names = FALSE)

# Are there any locations where females drink more than males?
# Your answer should be a *dataframe* of the locations, states, and differences for all locations (no extra columns)
filter(location.2012, difference < 0) %>% select(location, state, difference)
location.2012 %>% group_by(state,location) %>% summarise(diff = sum(difference)) %>% filter(diff < 0)

# What is the location in which male and female drinking rates are most similar (*absolute* difference is smallest)?
# Your answer should be a *dataframe* of the location, state, and value of interest (no extra columns)
filter(location.2012, difference == min(abs(difference))) %>% select(location, state, mean = difference)

# As you've (hopefully) noticed, the `location` column includes national, state, and county level estimates. 
# However, many audiences may only be interested in the *state* level data. Given that, you should do the following:
# Create a new variable that is only the state level observations in 2012
state.2012 <- filter(location.2012, state == location) %>% distinct()

# Which state had the **highest** drinking rate for both sexes combined? 
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)
filter(location.2012, both_sexes_2012 == max(both_sexes_2012)) %>% select(state, highest = both_sexes_2012)
highest.2012 <- filter(state.2012, both_sexes_2012 == max(both_sexes_2012)) %>% select(state, highest = both_sexes_2012)

# Which state had the **lowest** drinking rate for both sexes combined?
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)
lowest.2012 <- filter(location.2012, both_sexes_2012 == min(both_sexes_2012)) %>% select(state, lowest = both_sexes_2012)
lowest.2012 <- filter(state.2012, both_sexes_2012 == min(both_sexes_2012)) %>% select(state, lowest = both_sexes_2012)

# What was the difference in (any-drinking) prevalence between the state with the highest level of consumption, 
# and the state with the lowest level of consumption?
# Your answer should be a single value (a dataframe storing one value is fine)
highest.2012 %>% select(highest) - lowest.2012 %>% select(lowest)

# Write your 2012 state data to an appropriately named file in your `output/` directory
write.csv(state.2012, 'output/state.2012.csv', row.names = FALSE)

# Write a function that allows you to specify a state, then saves a .csv file with only observations from that state
# You should use the entire any.drinking dataset for this function
# Make sure the file you save in the `output` directory indicates the state name, and avoid using rownames.
StateInfo <- function(my.state) {
  result <- any_drinking %>% filter(state == my.state)
  write.csv(result, paste0('output/',my.state,'.csv'), row.names = FALSE)
}

# Demonstrate your function works by writing 3 .csv files of the states of your choice
StateInfo('Alabama')
StateInfo('Alaska')
StateInfo('Arizona')

################################### Binge drinking Dataset ###################################
# In this section, we'll ask a variety of questions regarding our binge.drinking dataset. 
# In order to ask these questions, you'll need to first prepare a subset of the data for this section:
  
# Create a dataframe with only the county level observations from the binge_driking dataset 
# (i.e., exclude state/national estimates)
# This should include "county-like" areas such as parishes and boroughs
counties <- binge_drinking %>% filter(location != state) %>% filter(location != "United States")

# What is the average county level of binge drinking in 2012 for both sexes?
summarise(counties, mean = mean(both_sexes_2012))

# What is the minimum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should contain 50 values (one for each state), unless there are two counties in a state with the same value
# Your answer should be a *dataframe* with the value of interest, location, and state
min.ineachstate <- counties %>% group_by(state) %>% summarise(location = first(location), min = min(both_sexes_2012))
#min.ineachstate <- counties %>% group_by(state) %>% filter(both_sexes_2012 == min(both_sexes_2012)) %>% select(state, location, both_sexes_2012) %>% distinct()

# What is the maximum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should be a *dataframe* with the value of interest, location, and state
counties %>% group_by(state) %>% summarise(location = first(location), max = max(both_sexes_2012))
counties %>% group_by(state) %>% filter(both_sexes_2012 == max(both_sexes_2012)) %>% select(state, location, both_sexes_2012)

# What is the county with the largest increase in male binge drinking between 2002 and 2012?
# Your answer should include the county, state, and value of interest
counties %>% mutate(increase = males_2012 - males_2002) %>% filter(increase == max(increase)) %>% select(location, state, increase)

# How many counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be an integer (a dataframe with only one value is fine)
male.increase <- nrow(counties %>% mutate(increase = males_2012 - males_2002) %>% filter(increase > 0))

# What percentage of counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)
male.total <- nrow(counties %>% mutate(increase = males_2012 - males_2002))
male.precent.increase <- round(male.increase / male.total * 100, 2)
                    
# How many counties observed an increase in female binge drinking in this time period?
# Your answer should be an integer (a dataframe with only one value is fine)
female.increase <- nrow(counties %>% mutate(increase = females_2012 - females_2002) %>% filter(increase > 0))

# What percentage of counties experienced an increase in female binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)
female.total <- nrow(counties %>% mutate(increase = females_2012 - females_2002))
female.precent.increase <- round(female.increase / female.total * 100, 2)

# How many counties experienced a rise in female binge drinking *and* a decline in male binge drinking?
# Your answer should be an integer (a dataframe with only one value is fine)
nrow(counties %>% mutate(increase.female = females_2012 - females_2002, increase.male = males_2012 - males_2002) %>% filter(increase.female > 0, increase.male < 0))
nrow(counties %>% mutate(increase.female = females_2012 - females_2002, increase.male = males_2012 - males_2002) %>% filter(increase.female > 0 & increase.male < 0))

################################### Joining Data ###################################
# You'll often have to join different datasets together in order to ask more involved questions of your dataset. 
# In order to join our datasets together, you'll have to rename their columns to differentiate them

# First, rename all prevalence columns in the any.drinking dataset to the have prefix "any."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.
#any_drinking
#setNames(any_drinking, paste0('any.',colnames(any_drinking)))
colnames(any_drinking) <- paste0('any.',colnames(any_drinking))

# Then, rename all prevalence columns in the binge.drinking dataset to the have prefix "binge."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.
colnames(binge_drinking) <- paste0('binge.',colnames(binge_drinking))

# Then, create a dataframe with all of the columns from both datasets. 
# You can do this by performing a full join on the two datasets by the `location` column
joined.driking <- any_drinking %>% left_join(binge_drinking, by = c('any.location' = 'binge.location', 'any.state' = 'binge.state'))

# Create a column of difference b/w `any` and `binge` drinking for both sexes in 2012
#joined.driking <- joined.driking %>% mutate(diff.both.sexes.2012 = any.both_sexes_2012 - binge.both_sexes_2012)
joined.driking <- joined.driking %>% mutate(diff.both.sexes.2012 = binge.both_sexes_2012 - any.both_sexes_2012)

# Which location has the greatest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)
joined.driking %>% select(any.state, any.location, diff.both.sexes.2012) %>% filter(abs(diff.both.sexes.2012) == max(abs(diff.both.sexes.2012)))

# Which location has the smallest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)
joined.driking %>% select(any.state, any.location, diff.both.sexes.2012) %>% filter(abs(diff.both.sexes.2012) == min(abs(diff.both.sexes.2012)))

################################### Write a function to ask your own question(s) ###################################
# Even in an entry level data analyst role, people are expected to come up with their own questions of interest
# (not just answer the questions that other people have). For this section, you should *write a function*
# that allows you to ask the same question on different subsets of data. 
# For example, you may want to ask about the highest/lowest drinking level given a state or year. 
# The purpose of your function should be evident given the input parameters and function name. 
# After writing your function, *demonstrate* that the function works by passing in different parameters to your function.

#drinking level given a state or year
output.info <- function(my.state, my.year) {
  filtered <- any_drinking %>% filter(any.state == my.state) %>% select(any.state, any.location, contains(my.year))
  return(filtered)
}

output.info('Alabama', '2012')

################################### Challenge ###################################

# Using your function from part 1 that wrote a .csv file for given a state name, write a file for all 50 states
# You should be able to do this in a *single line of (concise) code*
#min.ineachstate$state
lapply(min.ineachstate$state, StateInfo)

# Using a dataframe of your choice from above, write a function that allows you to specify a *year* and *state* of interest, 
# that saves a csv file with observations from that state's counties. 
# It should only write the columns `state`, `location`, and data from the specified year. 
# Before writing the .csv file, you should *sort* the data.frame in descending order
# by the both_sexes drinking rate in the specified year. 
# Again, make sure the file you save in the output directory indicates the year and state. 
# Note, this will force you to confront how dplyr uses *non-standard evaluation*
# Hint: https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
somefunction <- function(my.state, my.year) {
  temp <- paste0('both_sexes_', my.year)
  result <- any_drinking %>% filter(state == my.state) %>% select(location, state, contains(my.year)) %>% arrange_(desc(temp))
  #result <- any_drinking %>% filter(state == my.state) %>% select(location, state, contains(my.year)) %>% arrange_(paste0('both_sexes_', my.year))
  result <- any_drinking %>% filter(state == my.state) %>% select(location, state, contains(my.year)) %>% arrange_(paste0('both_sexes_', my.year))
  write.csv(result, paste0('output/',my.state, my.year,'.csv'), row.names = FALSE)
}

somefunction('Alabama', '2012')
somefunction('Alabama', '2002')
