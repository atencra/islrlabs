library(ISLR)
attach(Wage)


fit = lm(wage ~ poly(age,4),data=Wage)

coef(summary(fit))

fit2 = lm(wage ~ poly(age,4,raw=T),data=Wage)

coef(summary(fit2))


fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)

coef(fit2a)


fit2b = lm(wage ~ cbind(age,age^2,age^3,age^4),data=Wage)

agelims = range(age)

age.grid = seq(from=agelims[1],to=agelims[2])

preds = predict(fit,newdata=list(age=age.grid),se=TRUE)

se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit-2*preds$se.fit)


par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age,wage,xlim=agelims, cex=0.5, col="darkgrey")
title("Degree - 4 Poly", outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


preds2 = predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))



fit1= lm(wage~age ,data=Wage)
fit2= lm(wage~poly(age ,2) ,data=Wage)
fit3= lm(wage~poly(age ,3) ,data=Wage)
fit4= lm(wage~poly(age ,4) ,data=Wage)
fit5= lm(wage~poly(age ,5) ,data=Wage)
anova(fit1, fit2, fit3, fit4, fit5)


fit1 = lm(wage~education+age,data=Wage)
fit2 = lm(wage~education+poly(age,2),data=Wage)
fit3 = lm(wage~education+poly(age,3),data=Wage)
anova(fit1,fit2,fit3)


coef(summary(fit5))


fit = glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)

preds = predict(fit,newdata=list(age=age.grid),se=T)



pfit = exp(preds$fit) / (1 + exp(preds$fit))

se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit-2*preds$se.fit)

se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds = predict(fit,newdata=list(age=age.grid),type="response",se=T)



plot(age,I(wage>250), xlim=agelims, type="n",ylim=c(0,0.2))
points(jitter(age),I((wage>250)/5),cex=0.5,pch="|",
       col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)




# Fit a step function
table(cut(age,4))
fit = lm(wage~cut(age,4),data=Wage)
coef(summary(fit))


# Splines
library(splines)
fit = lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred = predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)

# Need to finish



# GAMs

# Fit GAM to predict wage

gm1 = lm(wage ~ns(year,4)+ns(age,5)+education,data=Wage)


library(gam)

gam.m3 = gam(wage~s(year,4)+s(age,5)+education,data=Wage)


par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")
plot.gam(gam1,se=TRUE,col="red")


gam.m1 = gam(wage~s(age,5)+education,data=Wage)
gam.m2 = gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

summary(gam.m3)


preds = predict(gam.m2,newdata=Wage)

gam.lo = gam(wage~s(year,df=4)+lo(age,span=0.7)+education,
      data=Wage)
plot.gam(gam.lo,se=TRUE,col="green")

gam.lo.i = gam(wage~lo(year,age,span=0.5)+education,
               data=Wage)

library(akima)
plot(gam.lo.i)


gam.lr = gam(I(wage>250)~year+s(age,df=5)+education,
             family=binomial,data=Wage)

par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education,I(wage>250))


gam.lr.s = gam(I(wage>250)~year+s(age,df=5)+education,
               family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")


# Eng of exercises
