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
  summarise(HighEd = mean(SCHL > 19, na.rm=TRUE)) %.% #Higher education Proportion
  arrange(desc(HighEd)) #Arranging in Descending order

#We find that Washington Leads with .2405,
#California second with .22998, and Oregon
#in last with .22726

names(CombinedStates_df)

#Finding the proportions within Each State of races
#with higher educations.
CombinedStates_df %.%
  filter(SCHL > 19) %.%
  group_by(State, SCHL) %.%
  summarise(HighEd.NotWhite.Prob=mean(RAC1P > 1, na.rm=TRUE))

CombinedStates_df %.%
  group_by(State) %.%
  summarise(NotWhite.Prop = mean(RAC1P > 1, na.rm=TRUE))

#How to find the mean for each of the education level
#for each state.
CombinedStates_df %.%
  filter(SCHL > 19) %.%
  group_by(State, SCHL) %.%
  summarise(MeanIncome = mean(WAGP, na.rm=TRUE)) 

#For Each state the highest mean income was for
#those who received a "Professional Degree beyond
# a Bachelor's". [the states are WA, CA, OR]

#It seems like the data may be skewed so using 
#mean as the measure of center could be throwing
#the data off. Therefore, median might be better
#[Also, I think we should look into how much of
#the data is missing!]

CombinedStates_df %.%
  filter(SCHL > 19) %.%
  group_by(State, SCHL) %.%
  summarise(MedIncome = median(WAGP, na.rm=TRUE), MeanIncome= mean(WAGP, na.rm=TRUE)) 
