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

#Lets use the variable tips

summary(dfTips$tip)
head(dfTips)
#define catgories: small tip<3 [0,3) meduim when tip is 3>= [3,7) but less than 7, 
#large otherwise when tip is 7 or more than 7 USD [7, 10]

#selection + Loop
catTips=c()  #create an empty vector

for (k in 1:length(dfTips$tip)) {
  if(dfTips$tip[k]<3){
    catTips[k]="AsmallTip"
  } else if (dfTips$tip[k] >=3 & dfTips$tip[k]<7) {
    catTips[k]="BmeduimTip"
  } else {
    catTips[k]="Clargetip"
  }
}

head(catTips)
dfnew=cbind(dfTips, catTips)
View(dfnew)
head(dfTips$tip)
#apply the function for FDT of QL
FDTQL(catTips)

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