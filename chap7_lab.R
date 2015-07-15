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


coef(summary(fit5))


fit = glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)

preds = predict(fit,newdata=list(age=age.grid),se=T)

pfit = exp(preds$fit) / (1 + exp(preds$fit))

se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit-2*preds$se.fit)

se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds = predict(fit,newdata=list(age=age.grid),type="response",se=T)


plot(age,I(wage>250), xlim=agelims, type="n",ylim=c(0,0.2))






