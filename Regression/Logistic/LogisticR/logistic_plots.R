library(AER)
data(Affairs,package="AER")
mydata<-Affairs
summary(mydata)
head(mydata)

table(mydata$affairs)

#Create binary variable affairs or maybe even children
mydata$naffair[mydata$affairs>0]<-1
mydata$naffair[mydata$affairs==0]<-0

fit.reduced<-glm(naffair~ age + yearsmarried + religiousness 
                 + rating, data=mydata,family=binomial())

mydata$prob<-predict(fit.reduced,type="response")

exp(coef(fit.reduced))

#Some test odds
test.data2<-data.frame(rating=c(1,2,3,4,5), age=mean(mydata$age),
                       yearsmarried=mean(mydata$yearsmarried),
                       religiousness=mean(mydata$religiousness))
test.data2$prob<-predict(fit.reduced,newdata=test.data2,type="response")
test.data2$odds<-(test.data2$prob/(1-test.data2$prob))
test.data2$decrease<-NA


for(i in 2:nrow(test.data2)){
  test.data2$decrease[i]<-1-((test.data2$odds[i-1]-test.data2$odds[i])/test.data2$odds[i-1])
  
}
test.data2


#Interesting what is it doing? 
Pi <- c(0.25, 0.5, 0.75)
LD <- (log(Pi /(1-Pi))-fit.reduced$coefficients[1])/fit.reduced$coefficients[5]
LD.summary <- data.frame(Pi , LD)
LD.summary


pi.hat <- exp(mydata$prob)/(1 + exp(mydata$prob))
LD.summary$group <- c('LD25','LD50','LD75')

library(ggplot2)
ggplot(mydata,aes(x = rating, y = prob)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0.1)) + 
  geom_line(aes(y = prob),colour = "black") + 
  geom_segment(data=LD.summary, aes(y = Pi,
                                    xend = LD,
                                    yend = Pi,
                                    col = group),x = -Inf,linetype = "dashed") + 
  geom_segment(data=LD.summary,aes(x = LD,
                                   xend = LD,
                                   yend = Pi,
                                   col = group),y = -Inf,linetype = "dashed")


