##The goals of this simulation are two-fold
	##We first try to quantify what is the increase in variation if sample size decreases from 16 to 14 (for logistcial purposes)
#Parameter values
a=200 # rnorm(1,20, 10)
b=6.9#rnorm(1, 20, 15)
c=-20.5#rnorm(1, 20, 15)

#Create empty vectors to fill in results
rec.t=NULL
rec.PH=NULL
rec.t.PH=NULL
for(j in 1:1000) #Outer forloop to repeat inner forloop 1000 times
{
ph=seq(from = 7.2, to = 8.4, by =0.17)	#ph range here is 8 so this is the rectangular design (2X8)
	t.sim=1
y0=b*ph+rnorm(length(ph), 0, 2)
y1=a*t.sim+b*ph+c*ph*t.sim+rnorm(length(ph), 0, 2) #Create y values based on parameter values, ph range, plus noise

y=c(y0, y1)
t=c(rep(0, 8), rep(1, 8))
PH=rep(ph, 2)
d1=data.frame(y, t, PH) #Create data frame to use for regression

d1.lm=lm(y~t*PH, data=d1) #This is the full regression with all 16 data points
plot(y~PH, data=d1,  main=paste(j)) #Plot for for loop = progress bar
	
d2=d1

results=NULL
for(i in 1:1000) #This is the second for loop where we randomly remove 2 values out of 16 and compare estimates
{
	results=rbind(results,(coef(d1.lm)[2:4]-coef(lm(y~t*PH, data=d2[-ceiling(runif(2, 1, 16)),]))[2:4])/(coef(d1.lm)[2:4])) #This takes the coefficient from the true lm (d1.lm) and subtract it from the coefficients from the lm where we removed 2 data points at random and find the proportional difference
 }
results=data.frame(results)

rec.t[j]=mean(abs(results$t)) #We take the mean absolute value of the second forloop to find the average proportional difference between the true lm and the lm where we remove 2 data points. We do this 1000 times. The difference between the 2 designs should be 0 on average so we take the absolute value to look at proportional differences (regardless of direction of change) instead
rec.PH[j]=mean(abs(results$PH))
rec.t.PH[j]=mean(abs(results$t.PH))
}

rec=data.frame(rec.t, rec.PH, rec.t.PH) #Create data frame for final result
mean(rec$rec.t)
mean(rec$rec.PH)
mean(rec$rec.t.PH)
hist(rec$rec.t)
library(boot) #I like using bootstrapping to find the confidence interval around the mean of the proportional difference. Useful to compare between the two designs
samplemean=function(x,d){return(mean(x[d]))}
b=boot(rec$rec.t, statistic=samplemean, R=10000) ##Around 3% difference compared to using 16 data points
plot(b)
b=boot(rec$rec.PH, statistic=samplemean, R=10000) ##Around 6% difference compared to using 16 data points
plot(b)
b=boot(rec$rec.t.PH, statistic=samplemean, R=10000) ##Around 4% difference compared to using 16 data points
plot(b) 
##On average, an increase in 5% spread of estimates if 2 data points are removed

###Let's do half the PH intervals but double the replication (This is the square design (4*4)). Are the results fromn this simulation meanginfully different than above?
a=200 # rnorm(1,20, 10)
b=6.9#rnorm(1, 20, 15)
c=-20.5#rnorm(1, 20, 15)
ph=seq(from = 7.2, to = 8.4, by =0.4)


##Same double forloop as above but with different design
sq.t=NULL
sq.PH=NULL
sq.t.PH=NULL
for(j in 1:100)
{
t.sim=1
ph=seq(from = 7.2, to = 8.4, by =0.4) #Here we only have 4 pH values but replicated
y0=c(b*ph+rnorm(length(ph), 0, 2), b*ph+rnorm(length(ph), 0, 2))
y1=c(a*t.sim+b*ph+c*ph*t.sim+rnorm(length(ph), 0, 2), a*t.sim+b*ph+c*ph*t.sim+rnorm(length(ph), 0, 2))
y=c(y0, y1)
PH=rep(ph, 4)
t=c(rep(0, 8), rep(1, 8))
d1=data.frame(y, t, PH)
plot(y~PH, data=d1, main=paste(j))

d3=d1
d3.lm=lm(y~t*PH, data=d3)
# summary(d3.lm)

results1=NULL
for(i in 1:1000)
{results1=rbind(results1,(coef(d3.lm)[2:4]-coef(lm(y~t*PH, data=d3[-ceiling(runif(2, 1, 16)),]))[2:4])/(coef(d3.lm)[2:4]))
 }
results1=data.frame(results1)
sq.t[j]=mean(abs(results1$t))
sq.PH[j]=mean(abs(results1$PH))
sq.t.PH[j]=mean(abs(results1$t.PH))
}

sq=data.frame(sq.t, sq.PH, sq.t.PH)
sq

mean(sq$sq.t)
mean(sq$sq.PH)
mean(sq$sq.t.PH)

# par(mfrow=c(2,2)) 
samplemean=function(x,d){return(mean(x[d]))}
s=boot(sq$sq.t, statistic=samplemean, R=10000) # 2.6% difference
plot(s)
s=boot(sq$sq.PH, statistic=samplemean, R=10000) #5% difference
plot(s)
s=boot(sq$sq.t.PH, statistic=samplemean, R=10000) #3$ difference
plot(s)

##Conclusion: surpisingly, doubling up on a certain pH level rather than spreading out seems to reduce variation a bit if 2 samples are removed. 
###Assumes trend with temperature is linear




