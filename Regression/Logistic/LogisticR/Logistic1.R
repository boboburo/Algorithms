#Illustrate the logit link function
p<-(1:9999)/10000     #Probability that the horse will win the race
gitp<-log(p/(1-p))    #Odds the horse will win the race

plot(p,gitp,xlab="Proportion",ylab="",type="l",pch=1)
plot(gitp,p,xlab="Proportion",ylab="",type="l",pch=1)

#The likelihood is the join probablity of the obsever data points, given the model parameters
#The deviance is twice the logarithm of the likelihood
#Maximixing the likelihood is equivalent to minimizing the deviance. 

likelihood<-(1:999)/1000
dev<- log(likelihood)*(-2)
plot(likelihood,dev,xlab="Likelihood",ylab="Deviance",type="l",pch=1)

######DATA######
library(DAAG)
df<-anesthetic
anestot<-aggregate(data[,c("move","nomove")],by=list(conc=data$conc),FUN=sum)
anestot   #is count of patients by the concentration level, 3 columns
anestot$conc <- as.numeric(as.character(anestot$conc))
anestot$total <- apply(anestot[, c("move","nomove")], 1 , sum) #new column
anestot$prop <- anestot$nomove/anestot$total #new column

 ## Plot proportion moving vs conc: data frame anesthetic (DAAG) 

plot(anestot$conc,anestot$prop,xlab="Concentration",ylab="Proportion", xlim = c(.5,2.5), ylim = c(0, 1), pch = 16)
plot(anestot$conc,log(anestot$prop),xlab="Concentration",ylab="Proportion", xlim = c(.5,2.5), ylim = c(0, 1), pch = 16)





######model#######
library(glm)
#Directly on the data
anes.logit <- glm(nomove~conc,family=binomial(link="logit"),data=df)
#or on the summarised data, not sure why the weights are provided here. 
anes1.logit<-glm(prop~conc,family=binomial(link="logit"),weights=total,data=anestot)

summary(anes.logit)
summary(anes1.logit)


fitted(anes.logit)
fitted(anes1.logit)

pre = data.frame(nomove=df$nomove,
                 fitted=fitted(anes.logit),
                 predicted=predict(anes.logit),
                 response=predict(anes.logit,type="response"),
                 link=predict(anes.logit,type="link"),
                 linkfit=predict(anes.logit, type="link", se.fit=TRUE)
                 )  




####### Manning R in ACTION BOOK ############ 
#### Affairs dataset######

library(AER)
data(Affairs,package="AER")
mydata<-Affairs
summary(mydata)
head(mydata)

table(mydata$affairs)

#Create binary variable affairs or maybe even children
mydata$naffair[mydata$affairs>0]<-1
mydata$naffair[mydata$affairs==0]<-0

head(mydata)
table(mydata$naffair)

model<-glm(naffair~gender + age + yearsmarried + children + religiousness + education 
           + occupation + rating, data=mydata,family=binomial(link="logit"))

summary(model)


#interpretting the model
#check for significance

fit1sw = step(model)  # Keeps all variables

library(car)
vif(model)
crPlots(model)

durbinWatsonTest(model)

library(ROCR)
fitpreds = predict(model,type="response")
fitpred = prediction(fitpreds,mydata$naffair)
fitperf = performance(fitpred,"tpr","fpr")
plot(fitperf,col="green",lwd=2,main="ROC Curve for Logistic:  Adult")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

library(Deducer)
rocplot(model)


??#In logistic, the predicted value is the log(odds that Y = 1, had affair)
#Scale is harld to understand
coef(model)
#Scale away the log with the exp. 
exp(coef(model))
## odds ratios and 95% CI
exp(cbind(OR = coef(model), confint(model)))
#Interpret the categorical variables
#The contrasts for race allow testing the following differences: 
#race=black      vs.race=white, 
#race=mexican    vs.race=white,  
#race=multi/othervs.race=white.


#Now years married increase by a 1 year, odds of affiar increase by 1.09, but age decreases it 0.95
exp(confint(model))

plot(predict(model,type="response"),residuals(model,type="deviance"))

#Diagnostic plots tend to be most useful whent he response variable takes on many values.
#When the repsonse variabelc an only take on a limited number of vlaues (binary), their 
#utility is decreased
plot(hatvalues(model))
plot(rstudent(model))
plot(cooks.distance(model))

library(car)
influencePlot(model)


#Testing the influence of the model 

test.data<-data.frame(rating=c(1,1,1,1,1), age=mean(mydata$age),
                      yearsmarried=c(5,5,5,5,5),
                      religiousness=mean(mydata$religiousness),
                      gender=c("male","male","male","male","male"),
                      children=c("yes","yes","yes","yes","yes"),
                      education=mean(mydata$education),
                      occupation=mean(1,1,1,1,1))

test.data$prob<-predict(model,newdata=test.data,type="response")
test.data



fit.reduced<-glm(naffair~ age + yearsmarried + religiousness 
                 + rating, data=mydata,family=binomial())

exp(coef(fit.reduced))

test.data2<-data.frame(rating=c(1,2,3,4,5), age=mean(mydata$age),
                      yearsmarried=mean(mydata$yearsmarried),
                      religiousness=mean(mydata$religiousness))
test.data2$prob<-predict(fit.reduced,newdata=test.data2,type="response")
test.data2$odds<-(test.data2$prob/(1-test.data2$prob))
test.data2$decrease<-NA


for(i in 2:nrow(test.data2)){
  test.data2$decrease[i]<-1-((test.data2$odds[i-1]-test.data2$odds[i])/test.data2$odds[i-1])
                                                                
}

hatvalues(fit.reduced)
rstudent(fit.reduced)
dd<-data.frame(hat=hatvalues(fit.reduced),
               rstudent=rstudent(fit.reduced),
               cooks=cooks.distance(fit.reduced))

h <- hatvalues(fit.reduced)
 plot(h, type="h")

model <- lm(Volume ~ Height * Girth, data=trees)
par(mfrow=c(2,2))  # set up a 2x2 matrix for the 4 diagnostic graphs
plot(fit.reduced)

par(mfrow=c(1,1)) 
str(pisaitems)

library(Hmisc)
describe(pisaitems)
