# a4-data-wrangling
################################### Set up ###################################

# Install (if not installed) + load dplyr package 
library(dplyr)

# Set your working directory to the appropriate project folder
setwd("~/GitHub/a4-data-wrangling-arihan-1560795")

# Read in `any_drinking.csv` data using a relative path
any.drinking <- read.csv('data/any_drinking.csv', stringsAsFactors = FALSE)

# Read in `binge.drinking.csv` data using a relative path
binge.drinking <- read.csv('data/binge_drinking.csv', stringsAsFactors = FALSE)

# Create a directory (using R) called "output" in your project directory
dir.create('output')

################################### Any drinking in 2012 ###################################
# For this first section, let's just work with the columns `state`, `location`, and the data from 2012
# (from the *any drinking* dataset)
# Create a data.frame that has the `state` and `location` columns, and all columns from 2012
any.drinking.2012 <- any.drinking %>% select(state, location, contains('2012'))

# Using the 2012 data, create a column that has the difference in male and female drinking patterns
any.drinking.2012 <- mutate(any.drinking.2012, 'male_minus_female' = males_2012 - females_2012)

# Write your 2012 data to a .csv file in your `output/` directory with an expressive filename
write.csv(any.drinking.2012, 'output/ 2012_any_drinking.csv', row.names=FALSE)

# Are there any locations where females drink more than males?
# Your answer should be a *dataframe* of the locations, states, and differences for all locations (no extra columns)
more.female.2012 <- any.drinking.2012 %>% filter(male_minus_female < 0) %>% select(location)

# What is the location in which male and female drinking rates are most similar (*absolute* difference is smallest)?
# Your answer should be a *dataframe* of the location, state, and value of interest (no extra columns)
similar.2012 <- any.drinking.2012 %>% 
  filter(male_minus_female == min(male_minus_female)) %>%
  select(location, state, male_minus_female)

# As you've (hopefully) noticed, the `location` column includes national, state, and county level estimates. 
# However, many audiences may only be interested in the *state* level data. Given that, you should do the following:
# Create a new variable that is only the state level observations in 2012
state.2012 <- any.drinking.2012 %>% filter(state == location)

# Which state had the **highest** drinking rate for both sexes combined? 
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)
highest.state.2012 <- state.2012 %>% 
      filter(both_sexes_2012 == max(both_sexes_2012)) %>%
      select(state, both_sexes_2012)

# Which state had the **lowest** drinking rate for both sexes combined?
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)
lowest.state.2012 <- state.2012 %>% 
  filter(both_sexes_2012 == min(both_sexes_2012)) %>%
  select(state, both_sexes_2012)

# What was the difference in (any-drinking) prevalence between the state with the highest level of consumption, 
# and the state with the lowest level of consumption?
# Your answer should be a single value (a dataframe storing one value is fine)
biggest.difference.2012 <- summarise(state.2012, max(both_sexes_2012)) - summarise(state.2012, min(both_sexes_2012))
 
# Write your 2012 state data to an appropriately named file in your `output/` directory
write.csv(state.2012, 'output/ 2012_any_drinking_state_data.csv', row.names=FALSE)

# Write a function that allows you to specify a state, then saves a .csv file with only observations from that state
# You should use the entire any.drinking dataset for this function
# Make sure the file you save in the `output` directory indicates the state name, and avoid using rownames.
save.state <- function(my.state){
  if(my.state != "National"){
      state.temp <- any.drinking %>% filter(state == my.state)
      write.csv(state.temp, paste('output/ ', my.state, '.csv', sep = ""))
  }
}

# Demonstrate your function works by writing 3 .csv files of the states of your choice
save.state("Washington")
save.state("Idaho")
save.state("Virginia")

################################### Binge drinking Dataset ###################################
# In this section, we'll ask a variety of questions regarding our binge.drinking dataset. 
# In order to ask these questions, you'll need to first prepare a subset of the data for this section:

# Create a dataframe with only the county level observations from the binge_driking dataset 
# (i.e., exclude state/national estimates)
# This should include "county-like" areas such as parishes and boroughs
not.state.binge <- binge.drinking %>%
      select(everything()) %>%
      filter(state != location & state != "National")

# What is the average county level of binge drinking in 2012 for both sexes?
avg.2012.both.sex.county <- mean(not.state.binge$both_sexes_2012)

# What is the minimum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should contain 50 values (one for each state), unless there are two counties in a state with the same value
# Your answer should be a *dataframe* with the value of interest, location, and state
min.county <- not.state.binge %>%
  group_by(state) %>%
  select(state, location, both_sexes_2012) %>%
  filter(both_sexes_2012 == min(both_sexes_2012))
  
# What is the maximum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should be a *dataframe* with the value of interest, location, and state
max.county <- not.state.binge %>%
  group_by(state) %>%
  select(state, location, both_sexes_2012) %>%
  filter(both_sexes_2012 == max(both_sexes_2012))

# What is the county with the largest increase in male binge drinking between 2002 and 2012?
# Your answer should include the county, state, and value of interest
not.state.binge <- mutate(not.state.binge, 'male_2012_minus_2002' = males_2012 - males_2002)
largest.male.drinking.increase.county.2002.2012 <- not.state.binge %>%
  select(state, location, male_2012_minus_2002) %>%
  filter(max(male_2012_minus_2002) == male_2012_minus_2002)

# How many counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be an integer (a dataframe with only one value is fine)
number.county.rise.male.drinking.2002.2012 <- nrow(filter(not.state.binge, male_2012_minus_2002 > 0))

# What percentage of counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)
percent.county.rise.male.drinking.2002.2012 <- nrow(filter(not.state.binge, male_2012_minus_2002 > 0)) / nrow(not.state.binge)
                    
# How many counties observed an increase in female binge drinking in this time period?
# Your answer should be an integer (a dataframe with only one value is fine)
not.state.binge <- mutate(not.state.binge, 'female_2012_minus_2002' = females_2012 - females_2002)
number.county.rise.female.drinking.2002.2012 <- nrow(filter(not.state.binge, female_2012_minus_2002 > 0))

# What percentage of counties experienced an increase in female binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)
percent.county.rise.female.drinking.2002.2012 <- nrow(filter(not.state.binge, female_2012_minus_2002 > 0)) / nrow(not.state.binge)

# How many counties experienced a rise in female binge drinking *and* a decline in male binge drinking?
# Your answer should be an integer (a dataframe with only one value is fine)
m.inc.f.d.2002.2012 <- nrow(not.state.binge %>% filter(not.state.binge$female_2012_minus_2002 > 0 & not.state.binge$male_2012_minus_2002 < 0))
                                  
################################### Joining Data ###################################
# You'll often have to join different datasets together in order to ask more involved questions of your dataset. 
# In order to join our datasets together, you'll have to rename their columns to differentiate them

# First, rename all prevalence columns in the any.drinking dataset to the have prefix "any."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.
colnames(any.drinking)[3:35] <- paste("any.", colnames(any.drinking[,c(3:35)]), sep = "")

# Then, rename all prevalence columns in the binge.drinking dataset to the have prefix "binge."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.
colnames(binge.drinking)[3:35] <- paste("binge.", colnames(binge.drinking[,c(3:35)]), sep = "")

# Then, create a dataframe with all of the columns from both datasets. 
# You can do this by performing a full join on the two datasets by the `location` column
master.data <- full_join(any.drinking, binge.drinking, by = c('location', 'state'))

# Create a column of difference b/w `any` and `binge` drinking for both sexes in 2012
master.data <- mutate(master.data, 'a.minus.b.mf.2012' = any.both_sexes_2012 - binge.both_sexes_2012)

# Which location has the greatest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)
biggest.diff.ab <- master.data %>%
  select(state, location, a.minus.b.mf.2012) %>%
  filter(max(abs(a.minus.b.mf.2012)) == a.minus.b.mf.2012)

# Which location has the smallest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)
smallest.diff.ab <- master.data %>%
  select(state, location, a.minus.b.mf.2012) %>%
  filter(min(abs(a.minus.b.mf.2012)) == a.minus.b.mf.2012)


################################### Write a function to ask your own question(s) ###################################
# Ask about the highest/lowest drinking level given a state or year. 
# The purpose of your function should be evident given the input parameters and function name. 
# After writing your function, *demonstrate* that the function works by passing in different parameters to your function.

top.10.drinking.counties <- function(my.state, my.year){
  binge.drinking.temp <- read.csv('data/binge_drinking.csv', stringsAsFactors = FALSE)
  data.temp <- binge.drinking.temp %>% 
    select(state, location, contains(paste('both_sexes_', my.year, sep=""))) %>%
    filter(state == my.state) %>%
    arrange_(paste0('desc(both_sexes_', my.year, ")", sep=""))
  return(head(data.temp, 10))
}

washington.2012 <- top.10.drinking.counties("Washington", 2012)
washington.2010 <- top.10.drinking.counties("Washington", 2010)

################################### Challenge ###################################

# Using your function from part 1 that wrote a .csv file for given a state name, write a file for all 50 states
# You should be able to do this in a *single line of (concise) code*
sapply(unique(master.data$state), function(x) save.state(x))

# Using a dataframe of your choice from above, write a function that allows you to specify a *year* and *state* of interest, 
# that saves a csv file with observations from that state's counties. 
# It should only write the columns `state`, `location`, and data from the specified year. 
# Before writing the .csv file, you should *sort* the data.frame in descending order
# by the both_sexes drinking rate in the specified year. 
# Again, make sure the file you save in the output directory indicates the year and state. 
# Note, this will force you to confront how dplyr uses *non-standard evaluation*
# Hint: https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
find.data <- function(my.year, my.state){
  any.drinking.c <- read.csv('data/any_drinking.csv', stringsAsFactors = FALSE)
  temp <- any.drinking.c %>% 
    filter(any.drinking.c$state == my.state) %>% 
    select(location, state, contains(my.year)) %>% 
    arrange_(paste0("desc(both_sexes_", my.year, ")"))
  write.csv(temp, paste('output/ ', my.state, '_' , my.year, '_any_drinking.csv', sep = ""))
}

find.data("2002", "Washington")

