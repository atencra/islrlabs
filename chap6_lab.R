library(ISLR)

fix(Hitters)

names(Hitters)

dim(Hitters)

sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)

dim(Hitters)

sum(is.na(Hitters))

library(leaps)

regfit.full = regsubsets(Salary~.,Hitters)

summary(regfit.full)

regfit.full = regsubsets(Salary~.,data=Hitters,nvmax=19)

reg.summary = summary(regfit.full)

names(reg.summary)

reg.summary$rsq

plot(reg.summary$rsq)


par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables",ylab="RSS",type="l")

plot(reg.summary$adjr2,xlab="# Vars", ylab="Adj RSq", type='l')
index_r2 = which.max(reg.summary$adjr2)
points(index_r2,reg.summary$adjr2[index_r2],col="red",cex=2,pch=20)

plot(reg.summary$cp, xlab="# Vars", ylab="Cp", type='l')
index_cp = which.min(reg.summary$cp)
points(index_cp,reg.summary$cp[index_cp],col='red',cex=2,pch=20)

index_bic = which.min(reg.summary$bic)
plot(reg.summary$bic,xlab='#Vars', ylab='BIC', type='l')
points(index_bic,reg.summary$bic[index_bic],col='red', cex=2, pch=20)


par(mfrow=c(2,2))
plot(regfit.full, scale='r2')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

coef(regfit.full,6)


# Forward and Backward Stepwise Selection

regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method='forward')
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method='backward')
summary(regfit.bwd)

coef(regfit.full,7)

coef(regfit.fwd,7)

coef(regfit.bwd, 7)


# Choosing among models using the validation set approach and cross-validation
set.seed(1)
train = sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)

test = !train

regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

test.mat = model.matrix(Salary~.,data=Hitters[test,])

val.errors = rep(NA,19)
for (i in 1:19){
  coefi = coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}

val.errors

which.min(val.errors)

coef(regfit.best,10)

predict.regsubsets = function(object,newdata,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object,id=id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}


regfit.best = regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(regfit.best,10)


k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters),replace=TRUE)
cv.errors = matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))


for (j in 1:k){
  best.fit = regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for (i in 1:19){
    pred = predict(best.fit,Hitters[folds==j,], id=i)
    cv.errors[j,i] = mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}



mean.cv.errors = apply(cv.errors,2,mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')


reg.best = regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)




#Ridge Regresssion and the Lasso

library(ISLR)
library(glmnet)

fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters))

Hitters = na.omit(Hitters)

x = model.matrix(Salary~.,Hitters)[,-1]

y = Hitters$Salary


# Fit Ridge Regression

grd = 10^seq(10,-2,length=100)

ridge.mod = glmnet(x,y,alpha=0, lambda=grd)

dim(coef(ridge.mod))

ridge.mod$lambda[50]

coef(ridge.mod)[,50]

sqrt(sum(coef(ridge.mod)[-1,50]^2))



ridge.mod$lambda[60]

coef(ridge.mod)[,60]

sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod, s=50, type='coefficients')[1:20,]

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grd,thresh=1e-12)

ridge.pred = predict(ridge.mod, s=4, newx = x[test,])

mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred = predict(ridge.mod, s=1e10,newx = x[test,])

mean((ridge.pred-y.test)^2)


ridge.pred = predict(ridge.mod, s=0, newx=x[test,],exact=T)

mean((ridge.pred - y.test)^2)

lm(y~x, subset=train)

predict(ridge.mod, s=0, exact=T, type="coefficients")[1:20,]



set.seed(1)

cv.out = cv.glmnet(x[train,], y[train],alpha=0)

plot(cv.out)

bestlam = cv.out$lambda.min
bestlam

ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])

mse_ridge = mean((ridge.pred-y.test)^2)

out = glmnet(x,y,alpha=0)

predict(out, type="coefficients",s=bestlam)[1:20,]


# Lasso
lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grd)
plot(lasso.mod)


set.seed(1)
cv.out = cv.glmnet(x[train,], y[train],alpha=1)
plot(cv.out)

bestlam = cv.out$lambda.min

lasso.pred = predict(lasso.mod,s=bestlam,newx=x[test,])

mse_lasso = mean((lasso.pred-y.test)^2)

out = glmnet(x,y,alpha=1,lambda=grd)

lasso.coef = predict(out,type="coefficients",s=bestlam)[1:20,]

lasso.coef

lasso.coef[lasso.coef !=0 ]

mse_ridge
mse_lasso













