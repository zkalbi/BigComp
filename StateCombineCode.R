#wagp (BW) Wage 
#schl (BR) [21+ means bachelor or higher] Education
#rac1p (DU) [9 categories] Race
#mar (AM) Marital Status
#esr (CM) [3 means unemployed] Employment Status

BW = 26*2 + 23
BR = 26*2 + 18
DU = 26*4 + 21
AM = 26 + 13
CM = 26*3 + 13
BW; BR; DU; AM; CM

State = c(rep("CA", 1097692), rep("OR", 116281), rep("WA", 206451))
length(State)

setwd("C:/Users/Martin/Documents/BigComp/SimpleData")
Combined = read.csv("StateCombined.csv", header=TRUE)
newComb = cbind(Combined, State)
write.csv(newComb, file="NewStateCombined.csv")

