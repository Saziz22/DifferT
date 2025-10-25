#Fall 25/ITC 255
#Descriptive methods
#Univar case 
#Numerical methods
#Center of distribution (mean, median, mode)

dfTips=read.csv("Samira_Azizi_ITC255.csv")
head(dfTips)


#check the distribution
plot(density(dfTips$Height..cm.))

#Locate the center....why is it important to locate the center
#different approaches
mean(dfTips$Height..cm.)  #
median(dfTips$Height..cm.)   #as a midpoint

mymode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mymode(dfTips$Height..cm.)

plot(density(dfTips$Height..cm.))

#Other locations (quantiles)
quantile(dfTips$Height..cm.)    #quartiles
quantile(dfTips$Height..cm., 0.7)
#write a fun that returns any location in the dist

myQnt=function(x,q){
  pr=quantile(x, q)
  return(pr)
}

myQnt(dfTips$Height..cm., 0.20)

##ECDF Emperical Cummulative Distribution Function

plot(ecdf(dfTips$Height..cm.), 
     col='blue', 
     main='ECDF of Tip', 
     xlab='tip')
abline(v=3.9, col='red', lty=3)
abline(h=0.8, col='darkgreen', lty=3)


ecdf(dfTips$Height..cm.)(170)
#41% are 170(cm) height  or shorter than. 

#quantile and ecdf are inverse of one another
quantile(dfTips$Height..cm., 0.8) #we have the percentage...look for the value

ecdf(dfTips$Height..cm.)(180)    #we have the value ...look for the percentage

# 75% are 180 cm height or smaller as height, 

#Outliers

boxplot(dfTips$Height..cm.,
        horizontal = T,
        col='#0033FF')

#outliers affect the location of the center dispropotionaly 
boxplot.stats(dfTips$Height..cm.)

#remove the outliers

HeightNew=dfTips$Height..cm.[dfTips$Height..cm.<175]

boxplot(HeightNew, horizontal = T)
mean(HeightNew)
median(HeightNew)
mymode(HeightNew)


plot(density(HeightNew))
#Variation
range(dfTips$Height..cm.)
sd(dfTips$Height..cm.)   
var(dfTips$Height..cm.)   #center means the mean
mad(dfTips$Height..cm.)   #Abs. value instead of square root
sd(HeightNew)

plot(density(dfTips$Height..cm.))

x=c(1,2,3,"5")
class(x)

#Next:Data Manipulation dplyr package
