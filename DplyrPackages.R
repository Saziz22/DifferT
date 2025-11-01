#ITC 255 
#data manipulation with dplyr package
install.packages()
library(dplyr)
install.packages("dplyr")
install.packages(c("ggplot2", "tidyverse"))
##a set of functions that perform certain actions

dfTips=read.csv('Samira_Azizi_ITC255.csv')
head(dfTips)
View(dfTips)
names(dfTips)
dim(dfTips)


#filter rows
fdata=filter(dfTips, Gender=='Female')
dim(fdata)
View(fdata)
mean(fdata$Age)

filter(dfTips, Gender !='Female')
head(fdata)
dim(fdata)


fNonS=filter(dfTips, Gender=="Female", Sport=='No', Age<25)
head(fNonS)
dim(fNonS)
View(fNonS)
median(fNonS$Age)

#logical operators &, |, ! AND OR NOT
#my dataset does not include the variable days.
#I tried with another variable but not works.
unique(dfTips$Level.of.Education)

LE=filter(dfTips, Level.of.Education=='MA'| Level.of.Education=='BA')  #day==Sat | Sun

head(weekend)
View(weekend)
dim(weekend)
nrow(dfTips)

dfw=filter(dfTips, day=='Sun' | day=='Fri')
head(dfw)
#weekend and female
wkEndF=filter(dfTips, (day=='Sun'|day=='Sat') & sex=='Female')
head(wkEndF)
dim(wkEndF)
View(wkEndF)
#Weekdays 
#TASK 1: filter those customers who visited not on a Weekend 
#and are Male and Paid a tip of more than x USD. 
#Specify x yourself PEN and PAPER Note 1=<tip<=10

unique(dfTips$day)

wkDays=filter(dfTips, day!='Sun' &  day !='Sat')
head(wkDays)
View(wkDays)

#use the function %in%
unique(dfTips$Age)

LA=filter(dfTips, Age %in% c(25,26))
head(LA)
# I do not have the variable day in my dataset.
WKELS=filter(dfTips, day %in% c('Sun','Sat') | size %in% c(5,6))
head(WKELS)
View(WKELS)

names(dfTips)
#for numerical >, <, ==
AHc=filter(dfTips, Age<28 & Height..cm.>177)
View(AHc)
x1=filter(dfTips, Age>=26)
x1
#TASK 2> filter those customers who visited on Fri, 
#paid a tip>x USD, their total Bill is smaller or equal to y USD 
#the size is smaller than 4 items. Specify x and y yourself

#Arrange
names(dfTips)
View(dfTips)
head(dfTips, 14)

head(arrange(dfTips, -desc(Age)))
head(arrange(dfTips, desc(Age)))
head(arrange(dfTips, desc(dfTips$Height..cm.)))   #decode the values F=1 Male 0

View(dfTips)
#select a subset based on column
names(dfTips)
xN=select(dfTips, c(Gender, Age, Height..cm.))
head(xN)
head(dfTips)

x1=select(dfTips, Gender, Age, everything())  #var no size is put at the beginning
head(x1)

head(dfTips)
dfTips1=select(dfTips, Gender:Age)
head(dfTips1)



head(select(dfTips, Sport:Age))
head(select(dfTips, -(Sport:Age)))

x2=select(dfTips, Sport:Height..cm.)
head(x2)
View(dfTips)


#rename
names(dfTips)

dfTips2=rename(dfTips, Sex=Gender)
head(dfTips2)


#mutate
head(dfTips)
#I combined Age with Height
dfTips3=mutate(dfTips, CAgeHeight=Age+Height..cm.)
View(dfTips3)
dfTips3=mutate(dfTips3, SuperGigantism=Height..cm.*2)
View(dfTips3)
#summarize for numerical vars
summarise(dfTips, mean(Age), sd(Age),  mean(Height..cm.), sd(Height..cm.))

#based on another variable
GG=group_by(dfTips, Gender)

summarise(GG, mean(Age), sd(Age))

Sport=group_by(dfTips, Sport)
summarise(Sport, mean(Age), sd(Age))


Level.of.Education=group_by(dfTips3, Level.of.Education)
summarise(Level.of.Education, mean(Height..cm.), median (Height..cm.), sd(Height..cm.))
names(dfTips)

names(dfTips3)
#pull a column as a vector 
spt = pull(dfTips, Sport)

head(spt)

#sample_n works with rows
dim(dfTips)
sampledfTips=sample_n(dfTips, 10)
head(sampledfTips)
View(sampledfTips)
