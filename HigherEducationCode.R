#An Attempt at the code for Identifying which state has
#the largest proportion of people who have recieved a 
#bachelors degree or higher

library(dplyr)

#Reading in the Data
newComb = read.csv("NewStateCombined.csv", header=TRUE)

#Chaning the Data into a tabled dataframe
CombinedStates_df <- tbl_df(newComb)

#Applying the dplyr commands to find the highest proportions
CombinedStates_df %.% #Specifying the dataframe
  group_by(State) %.% #Grouping by State
  summarise(HighEd = mean(SCHL > 20, na.rm=TRUE)) %.% #Higher education Proportion
  arrange(desc(HighEd)) #Arranging in Descending order

#We find that Washington Leads with .2405,
#California second with .22998, and Oregon
#in last with .22726
