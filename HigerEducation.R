
## I created two forms of data, AllStates.csv and AllStates.txt. Use whichever you
## prefer

##  Variable code:
# SCHL: educational attainment, bb, 1-24  
# MAR: marital Status  1-5
# WAGP: Wage or salary income
# RAC1P: race,  1-9. 

setwd("/home/zhuob/BigComp/SimpleData")
big <- read.table("AllStates.txt", header=T)
dim(big)  # 9286739       6

# loading dplyr
library(dplyr)

#Chaning the Data into a tabled dataframe
All_df <- tbl_df(big)


new <- All_df %.%  # Specifying the dataframe
  group_by(ST) %.% #Grouping by State
  summarise(HighEd = mean(SCHL > 20, na.rm=TRUE)) %.% #Higher education Proportion
  arrange(ST) # Arranging in ascending order

State <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
  "HI", "ID", "IL", "IN", "IA", "KS","KY", "LA", "ME", "MD", "MA", "MI", "MN", 
  "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR"
  , "PA", "RI", "SC", "SD", "TN",  "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")




#Finding the proportions within Each State of races
#with higher educations.
new2 <- All_df %.%
  filter(SCHL > 19) %.% # Associates degree or above
  group_by(ST) %.%
  summarise(HighEd.White.percent=mean(RAC1P == 1, na.rm=TRUE))


new3 <- All_df %.%
filter(SCHL > 19) %.%
  group_by(ST) %.%
  summarise(HighEd.NotWhite.percent=mean(RAC1P > 1, na.rm=TRUE))%.%
  arrange(ST)

summary.dat <- cbind(new, new2, new3, State)


# want to see among white people, what percentage of them receive higher education
White.HighEd <- All_df%.%
  filter(RAC1P == 1)%.% 
  group_by(ST)%.%
  summarise(HighEd.White.percent= mean(SCHL > 19, na.rm= TRUE))

# Among non-white people,  what percentage of them receive higher education
Nonwhite.HighEd <- All_df%.%
filter(RAC1P == 6)%.% 
  group_by(ST)%.%
  summarise(HighEd.White.percent= mean(SCHL > 19, na.rm= TRUE))

## define a function for summarizing
#  All you need is to specify the race category (from 1 -9)

race <- function(x)  # x defines the race group 
{ table <- All_df%.%
    filter(RAC1P == x)%.%
    group_by(ST)%.%
    # round to the third decimal place
    summarise(percent=round(mean(SCHL > 19, na.rm=TRUE), 3))%.%
    arrange(ST)
  RAC1P <- rep(x, dim(table)[1])
  # create a table containg the race code
  return(cbind(table, RAC1P, State)) 
}

# example: I want to see what portion of white people with higher education
cbind(race(1), race(6))

# draw some figures
library(ggplot2)

white.hi <- race(1)

names(white.hi)
plot(white.hi$perce)



