#Fall 25/ITC 255
#Descriptive methods
#Univar case 
#FDT and basic Graphs

#Upload the data data set tips

dfTips=read.csv("Samira_Azizi_ITC255.csv")
View(dfTips)

#FDT of a QL var
names(dfTips)
View(dfTips)
#Gender Distribution

AbsFreq=table(dfTips$Gender)
AbsFreq
prop.table(AbsFreq)    #Abs. Freq
RelFreq=round(prop.table(AbsFreq), 2)
RelFreq

CumFreq=cumsum(RelFreq)
CumFreq

FDTGender=cbind(AbsFreq, RelFreq, CumFreq)
FDTGender

#write a function that creates and FDT of a QL var


FDTQL=function(x){
  ABSFreq=table(x)
  RELFreq=round(prop.table(ABSFreq),2)
  CUMFreq=cumsum(RELFreq)
  FDTx=cbind(ABSFreq, RELFreq, CUMFreq)
  return(FDTx)
}

FDTQL(dfTips$Gender)

FDTQL(dfTips$Sport)
FDTQL(dfTips$Level.of.Education)

##Construction FDT of a Quant variable 
#Loops and conditional functions work in R
#1. Transform the variable into a categorical var based a definition/we specify them

#Lets use the variable Age.

summary(dfTips$Age)
head(dfTips)
#define catgories: small Age<20 [15,20) meduim when Age is 20>= [20,25) but less than 25, 
#large otherwise when Age is 25 or more than 25 years [25, 40]

#selection + Loop
catAges=c()  #create an empty vector

for (k in 1:length(dfTips$Age)) {
  if(dfTips$Age[k]<20){
    catAges[k]="AsmallAge"
  } else if (dfTips$Age[k] >=20 & dfTips$Age[k]<25) {
    catAges[k]="BmeduimAge"
  } else {
    catAges[k]="ClargeAge"
  }
}

head(catAges)
dfnew=cbind(dfTips, catAges)
View(dfnew)
head(dfTips$Age)
#apply the function for FDT of QL
FDTQL(catAges)

#++++++++++++++++++++Descriptive methods++++++++++++
#Univar case 
#Graphs 
#Categorical vars (pie and bar)

#create the FDT 
FDTQL(dfTips$smoker)[,2]

fdtSmoker=FDTQL(dfTips$smoker)[,2]
fdtSmoker

pie(fdtSmoker, 
    col = rainbow(2), 
    main = 'Smoker Distribution')

barplot(fdtSmoker, 
        col=rainbow(2), 
        main = 'Smoker distribution')

fdttip=FDTQL(catTips)[,2]
fdttip

barplot(fdttip, 
        col=rainbow(3), 
        main = 'Tip distribution')

#Descriptive methods
#Univariate case 
#Graphs 
#Numerical vars (hist and density)

head(dfTips)

hist(dfTips$tip, 
     col='blue', 
     main = 'Tips distibution')

plot(density(dfTips$tip), 
     col='#0033FF', 
     main='Tips distribution')


plot(density(dfTips$total_bill), 
     col='#0033FF', 
     main='Total Bill distribution')

y=read.csv("timeToOffice.csv")
names(y)


hist(y$T)
plot(density(y$T))