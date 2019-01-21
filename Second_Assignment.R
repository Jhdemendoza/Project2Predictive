################################SET WORKING DIR###########################
#setwd("/Users/Jaime/Desktop/Master/PredictiveModeling/Project2/")
setwd("C:/Users/alvaro/Documents/GitHub/Predictive2/Project2Predictive")


################################IMPORTING LIBRARIES#######################
library("psych")
library("MASS")
library("scatterplot3d")
library("pracma")
library("tidyverse")
library("car")
library("glmnet")
library("ISLR")

##################################READ DATA##########################
admision = read.csv("Admission_Predict.csv",header = TRUE)
summary(admision)
# There are no missing values
# Get rid of serial number since it doesn't really mean anything
admision=admision[,c(-1)]
admision$Research=as.factor(admision$Research)
options(warn=-1)
names(admision)
names(admision)=c("GRE","TOEFL","UniRating","SOP","LOR","CGPA","Research","Chance")
attach(admision)

dim(admision)
sapply(admision,class)
my_cols=c("red","blue")

############################EXPLORATORY ANALYSIS#####################
pairs.panels(admision, 
             method = "pearson", # correlation method
             hist.col = "#00146E",
             col = "red",
             lm = FALSE,
             ellipses = FALSE,
             smooth = FALSE,
             pch = c(21,21),
             bg=my_cols[as.factor(Chance>0.9)],
             rug = FALSE,
             cex.cor = 5,
             scale = TRUE,
             density = TRUE  # show density plots
)
# All variables but Research follow a Gaussian distribution
# The most correlated variables against Chance (which is the target variable) are GRE
# and CGPA which could be understood as equivalent to the PAU in Spain
names(admision)

{
var=Chance
name="Chance"
summary(var)
boxplot(var)
text(x=1.3,y=boxplot(var)$stats,labels = boxplot(var)$stats)
text(x=0.65,y=mean(var),labels = paste("Mean:",round(mean(var),1)))
plot(density(var),main="",xlab = name)
abline(v=mean(var))
}

# The parameters included are : 
## 1. GRE Scores (out of 340) GRE Quantitative variable
summary(GRE)
boxplot(GRE)
text(x=1.3,y=boxplot(GRE)$stats,labels = boxplot(GRE)$stats)
text(x=0.65,y=mean(GRE),labels = paste("Mean:",round(mean(GRE),1)))
plot(ecdf(GRE))
plot(density(GRE),main="",xlab = "GRE")
abline(v=mean(GRE))
## No outliers for GRE and as seen in the first plot it follows a Normal distribution

## 2. TOEFL Scores (out of 120) TOEFL Quantitative variable
summary(TOEFL)
boxplot(TOEFL)
plot(ecdf(TOEFL))
plot(density(TOEFL))
## No outliers for TOEFL and as seen in the first plot it follows a Normal distribution

## 3. University Rating (out of 5) UniRating Qualitative variable (it classifies Universities
## according to how good they are)
summary(UniRating)
boxplot(UniRating)
#admision$UniRating <- as.factor(UniRating)
# Does'nt make sense in qualitative variables as much as in continuous.
plot(ecdf(UniRating))
plot(density(UniRating))
## No outliers for UniRating and as seen in the first plot it follows a Normal distribution

## 4. Statement of Purpose (out of 5 every 0.5) SOP Quantitative variable that measures the goodnes of
## the statement of purpose
summary(SOP)
boxplot(SOP)
#admision$SOP <- as.factor(SOP)
# Does'nt make sense in qualitative variables as much as in continuous.
plot(ecdf(SOP))
plot(density(SOP))
## No outliers for SOP and as seen in the first plot it follows a Normal distribution

## 5. Letter of Recommendation Strength (out of 5 every 0.5) LOR Quantitative variable (Very similar to 
## the previous one but concerning letters of recommendation)
summary(LOR)
boxplot(LOR)
#admision$LOR <- as.factor(LOR)
# Does'nt make sense in qualitative variables as much as in continuous.
plot(ecdf(LOR))
plot(density(LOR))
## No outliers for LOR and as seen in the first plot it follows a Normal distribution

## 6. Undergraduate GPA (out of 10) CGPA Quantitative variable
summary(CGPA)
boxplot(CGPA)
# Does'nt make sense in qualitative variables as much as in continuous.
plot(ecdf(CGPA))
plot(density(CGPA))
## This variable is a little bit skewed, but could be assumed to follow a Normal distribution

## 7. Research Experience (either 0 or 1) Research Binary variable
summary(Research)
## Pretty much divides the data into two groups
### ************ Could be interesting to see how this variable changes the different models

## 8. Chance of Admit (ranging from 0 to 1) Chance Quantitative variable
summary(Chance)
boxplot(Chance)
plot(ecdf(Chance))
plot(density(Chance))
## This variable, as is CGPA, is a little bit skewed, but it could be assumed to follow
## a Normal distribution. Also, since this is going to be our target variable, we will
## make it a binary one by using a cutoff.
attach(admision)

summary(Research)

################################LINEAR REGRESSION#########################
Anova(lm(Chance~.,data = admision))
## In reality Chance is a continuous variable so it is interesting to see from what
## variables it could be predicted.
## Computing the anova table suggests that the most important variables are GRE, TOEFL, LOR, CGPA 
## and Research.
## Let's see what the best linear model is according to the BIC just to see what are the most
## important variables.
model <- stepAIC(lm(Chance~.,data=admision), k = log(length(Chance)))
summary(model)
## The important variables are those that we expected from the ANOVA table.
## Just with this model, we could predict the probability of getting accepted into 
## a certain university, but it needs a lot of variables.
##
## It would be interesting to set a cutoff such that the model would just answer that
## you could get accepted into a certain university with a high level of confidence
## but needing less variables in order to calculate it.
## The reason behind why it would be interesting to have less variables is because
## if you are planning on applying to a University you would have to get Letters of 
## recommendation, write the statement of purpose and probably pay a fee.
## Therefore it is of high interest trying to answer the question "Will I get accepted?"
## on the minimum number of variables or what it is the same "loosing" the minimum amount
## of time possible.



#############################LOGISTIC REGRESSION###########################
## For the reasons given before we will be building some classification models, where
## the class will be given by Chance>0.9, i.e. having more than 90% probability of
## being accepted.
plot(density(Chance))
abline(v=0.9)

cutoff=0.9
summary(Chance > cutoff)
## As we can see this cutoff makes two partitions, one of them contains 12,25% of the data points
## and the other one the rest.

#### Let's start by looking at Chance vs. each one of the variables
diff_model<-function(response,predictor,name="",data=admision,gr=FALSE){
  predictor_scaled=scale(predictor)
  mod <- glm(response>cutoff~predictor_scaled,family = "binomial",data=admision)
  if (gr == TRUE) {
    x_plot <- seq(-3,3,by=0.1)
    temp <- mod$coefficients[1]+mod$coefficients[2]*x_plot
    x_plot <- x_plot*sd(predictor)+mean(predictor)
    plot(x=x_plot,y=logistic(temp),type = "line",xlab=name,ylab = "Probability")
    points(x=predictor,y=response>cutoff)
    x_d <- (-mod$coefficients[1]/mod$coefficients[2])*sd(predictor)+mean(predictor)
    y_d <- 0.5
    points(x_d,y_d,pch=19,col="blue")
    text(x_d,y_d,labels = round(x_d,2),pos=4)
  }
  tab <- table(Chance>cutoff,mod$fitted.values>0.5)
  print(tab)
  accuracy <- sum(diag(tab)) / sum(tab)
  tnr <- tab[1]/sum(tab[,1])
  tpr <- tab[4]/sum(tab[,2])
  print(name)
  print(paste("Accuracy:",accuracy))
  print(paste("True positive rate:",tpr))
  print(paste("True negative rate:",tnr))
}

diff_model(Chance,GRE,"GRE", gr=TRUE)
diff_model(Chance,TOEFL,"TOEFL", gr=TRUE)
diff_model(Chance,as.numeric(UniRating),"UniRating", gr=TRUE)
diff_model(Chance,SOP,"SOP", gr=TRUE)
diff_model(Chance,LOR,"LOR", gr=TRUE)
diff_model(Chance,CGPA,"CGPA", gr=TRUE)
#diff_model(Chance,Research,"Research", gr=TRUE)
# As we can see we can be more certain that the classifier has returned a correct answer
# when it says that we will not me admitted because the TNR is bigger than the TPR.
# There variables have the produce best TPR's are CGPA, GRE and TOEFL.
# We could also see what happens when using polynomial predictors

##diff_model(Chance,Research+GRE,"aa")
indexes <- c(1,2,4,5,6)
for (i in 1:(length(indexes))) {
  for (j in 1:(length(indexes))) {
    if (j > i) {
        diff_model(Chance,admision[indexes[i]]+admision[indexes[j]], paste(names(admision)[indexes[i]],"+",names(admision)[indexes[j]]))
    }
  }
}


###################################BEST MODEL ACCORDING TO BIC######################################
mod <- stepAIC(glm(Chance > cutoff ~ ., data = admision, family = "binomial"), k = log(length(Chance)))
summary(mod)
pairs.panels(admision[,c(3,6,7,8)], 
             method = "pearson", # correlation method
             hist.col = "#00146E",
             col = "red",
             lm = FALSE,
             ellipses = FALSE,
             smooth = FALSE,
             rug = FALSE,
             cex.cor = 5,
             scale = TRUE,
             density = TRUE  # show density plots
)
tab <- table(Chance>cutoff,mod$fitted.values>0.5)
print(tab)
accuracy <- sum(diag(tab)) / sum(tab)
tnr <- tab[1]/sum(tab[,1])
tpr <- tab[4]/sum(tab[,2])
print(paste("Accuracy:",accuracy))
print(paste("True positive rate:",tpr))
print(paste("True negative rate:",tnr))
### For this it would be interesting to see how an observarion changes if it has done research
### and if it hasn't with the predict

## Allowing for interactions returns the same result -> The important predictors are GRE, LOR and CGPA
mod_int <- stepAIC(glm(Chance > cutoff ~ .^2, data=admision, family="binomial"), k = log(length(Chance)))
summary(mod_int)
## The interactions model just returns the same as the model that doesn't allow for interactions

summary(mod)
confint.default(mod, level=0.95)
exp(confint.default(mod, level=0.95))
## From the confidence intervals at alpha = 0.05 we can see that neither the intercept nor the
## research are significant at that interval.
## The interpretation of the intercept not being significant is that when UniRating = 0, CGPA = 0
## and Research1 = 0, the probability of being accepted is not significantlly larger than
## not being accepted.
## UniRating and CGPA are significant, this means that an increase in UniRating of one unit, makes
## the odds of getting accepted increase by a factor between 1.178 and 12.519
## For CGPA, which is more signiticant than UniRating, an increase in one unit of CGPA makes the odds
## increase by a factor between 7167 and 1086185000.

## In principle the UniRating should be negatively correlated with the probability of being accepted
## because it is harder to get into a better University. In this case, the UniRating is correlated
## positively. This is probably due to the fact, that people normally has an inctuition of whether 
## they are going to get accepted or not, and people with lower GPAs doesn't try to go to UniRating
## Maybe for this reason, we should get rid of this variable, or make a more in depth analysis ->
## -> Make a different model for each rating in order to see how it varies, or 3 models (UniRating = 1&2,
## UniRating = 3&4 and UniRating = 5)

## Interpretations for the model (regarding the odds:
# Taking into account that we are currently on January
# and we have enough time to prepare well just one exam, 
# which one should be the best option - check which are the higher B's


#################################ANALYSING DEVIANCE##############################
mod$deviance
mod$null.deviance
R_squared=1-(mod$deviance/mod$null.deviance)
R_squared
#it is a proportion of how good the fit is compared to the worst

mod$anova
#From this result we can realize that model performs 
#better reducing the number of predictors from the
#model with all predictors included, reducing 
#deviance and AIC at the same time


#Deviance measures the deviance of the fitted generalized 
#linear model with respect to a perfect model for 
#E[YjX1 = x1, . . . , Xp = xp]. This perfect model, 
#known as the saturated model
#What we need is to reduce the deviance
#The deviance is a generalization of the Residual Sum of
#Squares (RSS) of the linear model. The generalization is
#driven by the likelihood and its equivalence with the RSS
#in the linear model.

############################MODEL DIAGNOSIS########################
#Checking normality
idx=mod$residuals>-10

summary(Chance==0.9)
mod$residuals[34]
mod$fitted.values[34]
Chance[34]
plot(mod$residuals)
#I do not know why there is an outlier of -147...

plot(mod,1)
#From the graph above we can verify that there is no trend
#on the residuals vs the fitted values, which lead us 
#to conclude that there is linearity between the transform
#expectation of Y and the predictors
#Here we have to be careful cause with the term
#residuals, we are refering to the deviance residuals which 
#are the generalization of the residuals from a linear model.

#Checking Distribution
plot(mod,2)
#As expected, normality in the deviance residuals is met
#as the normal qq-plot fits the line 
#(talking into account that some difference is expected in the tails)
#Std.deviance vs theoretical Quantile fits an horizontal line due to for most of the points,
#Standard deviance residual is 0. As we are trying to predict a binomial
#variance, this result make sense. This also lead us to conclude that
#most of the residual has to follow a very thick hood
plot(density(mod$residuals[mod$residuals>-10]+1))


#Checking Independece 
plot(mod$residuals[mod$residuals>-10],type = "line")
Chance[abs(mod$residuals)>1.2]
#Independe can be assure. Apparently, there exist a kind of memmory
#from one point to the following. This is not a problem in this case, 
#as we are applying a Binomial distribution so this result is expected
#Higher values of residuals correspond to the points closest
#to the boundary set by the cutoff, which make sense cause are 
#harder to predict

{min(mod$residuals)
  mod$residuals[32:35]
  head(admision[32:35,])
  temp_1=predict(mod,admision[32:35,])
  temp_1
  logistic(temp_1)}
#Here we can observe a kind of residual-outlier for obervation 34
#After checking, we can verify that this point does not affect 
#to the assumption of indepence, so we can confirm that assumption 


#Checking multicollinearity
car::vif(mod)
#VIF looks into linear relation between predictors. 
#From this result we can be sure that there is no multicollinearity
#between predictors. 

names(mod$coefficients)
names(admision)
pairs(admision[,c(3,6,7)])
#Not good idea to plot results this way


############################Ridge & Lasso##################
#Básicamente, si aumento la lambda penalizo la deviance del modelo. 
#El mejor modelo para ambos casos lo obtenemos cuando lambda = 0
#Lo que respalda lo anterior.
#Con lo que, lo mejor sería plantear distintas gráficas, viendo como
#afectan las lambdas al modelo y así llegar a un punto de equlibrio
#con lasso seguramente, donde encontremos que nos hemos
#quitado predictors (simplifico el modelo) pero aún así
#tenemos un buen resultado en deviance!

y<-admision$Chance>0.9
x<-model.matrix(Chance>0.9~.,data=admision)[,-c(1)]
x=apply(X = x,MARGIN = 2,FUN = scale)

#Initializing some arrays
{
  steps=seq(from=0,to = .3,by = 0.01)
  accuracy=rep(0,length(steps))
  tnr=rep(0,length(steps))
  tpr=rep(0,length(steps))
  fpr=rep(0,length(steps))
  deviance=rep(0,length(steps))
  r_squared=rep(0,length(steps))
  j=1
}
#Iterating over lambda - 
#Lambda > 0.3 evaluates everything as negative
for (i in steps){
  ridgeMod <- glmnet(lambda = i,x = x, y = y, alpha = 0, family = "binomial")
  deviance[j]=(1-ridgeMod$dev.ratio)*ridgeMod$nulldev
  r_squared[j]=ridgeMod$dev.ratio
  val_pre=logistic(x=predict(ridgeMod,newx = x))
  tab <- table(Chance>cutoff,val_pre>0.5)
  print(tab)
  accuracy[j]<- sum(diag(tab)) / sum(tab)
  tnr[j] <- tab[1]/sum(tab[,1])
  tpr[j] <- tab[4]/sum(tab[2,])
  fpr[j] <- tab[2]/sum(tab[1,])
  print(paste("Accuracy:",accuracy[j]))
  print(paste("True positive rate:",tpr[j]))
  print(paste("True negative rate:",tnr[j]))
  print(paste("False positive rate:",fpr[j]))
  j=j+1
}
#Here we make some plots to explain the model
{
par(mfrow=c(1,4))
plot(r_squared,x=steps,type="line",main="R Squared")
plot(main="Deviance Analysis",x=steps,y=deviance,xlab = "Lambda",ylab = "Deviance",type="line")
plot(main="Accuracy of the predictions",accuracy,x=steps,xlab = "lambda",type="line",ylim = c(0.7,1),xlim = c(0,.3))
#points(y=tnr,x=steps,type="line",col="red")
plot(main="ROC Curve",y=tpr,x=fpr,xlim = c(0,0.15),ylim = c(0,1),type="line")
dev.off()
}
#Main conclusions that can be obtained from here:
##Lambda penalizes the results of the model. Best results
###are obtained if Lambda=0!
##In that case, R squared is 0.84
##Deviance absolute result is 45.42
##Accuracy is 0.975
## tpr = 0.89 while fpr=.01

{
fpr[1]
tpr[1]
r_squared[1]
accuracy[1]
deviance[1]
}

ridgeMod <- glmnet(x = x, y = y, alpha = 0, family = "binomial")
plot(ridgeMod, label = TRUE, xvar = "lambda")
abline(v=log(0.3))
#From this plot we can see that model with
#lambdas closer to 0.3 performs almost 
#(recall that variables are normalize)
#if every variable has the same significance

plot(y=ridgeMod$dev.ratio,x=log(ridgeMod$lambda))
#Deviance explained by the model decreases as we 
#penalize the value of the B's - which make sense cause
#if all b's are close to 0 we are predicting using only B0-a constant - i.e. null deviance!

###################LASSO####################
#Initializing some arrays
{
  steps=seq(from=0,to = 0.1,by = 0.01)
  accuracy=rep(0,length(steps))
  tnr=rep(0,length(steps))
  tpr=rep(0,length(steps))
  fpr=rep(0,length(steps))
  deviance=rep(0,length(steps))
  r_squared=rep(0,length(steps))
  j=1
}
#Iterating over lambda - 
#Lambda > 0.1 evaluates everything as negative
for (i in steps){
  lassoMod <- glmnet(lambda = i,x = x, y = y, alpha = 1, family = "binomial")
  deviance[j]=(1-lassoMod$dev.ratio)*lassoMod$nulldev
  r_squared[j]=lassoMod$dev.ratio
  val_pre=logistic(x=predict(lassoMod,newx = x))
  tab <- table(Chance>cutoff,val_pre>0.5)
  print(tab)
  accuracy[j]<- sum(diag(tab)) / sum(tab)
  tnr[j] <- tab[1]/sum(tab[,1])
  tpr[j] <- tab[4]/sum(tab[2,])
  fpr[j] <- tab[2]/sum(tab[1,])
  print(paste("Lambda:",i))
  print(paste("Accuracy:",accuracy[j]))
  print(paste("True positive rate:",tpr[j]))
  print(paste("True negative rate:",tnr[j]))
  print(paste("False positive rate:",fpr[j]))
  j=j+1
}
#Here we make some plots to explain the model
{
  par(mfrow=c(1,4))
  plot(r_squared,x=steps,type="line",main="R Squared")
  plot(main="Deviance Analysis",x=steps,y=deviance,xlab = "Lambda",ylab = "Deviance",type="line")
  plot(main="Accuracy of the predictions",accuracy,x=steps,xlab = "lambda",type="line",ylim = c(0.7,1),xlim = c(0,.3))
  #points(y=tnr,x=steps,type="line",col="red")
  plot(main="ROC Curve",y=tpr,x=fpr,xlim = c(0,0.15),ylim = c(0,1),type="line")
  
}
lassoMod <- glmnet(x = x, y = y, alpha = 1, family = "binomial")
plot(lassoMod,label = TRUE, xvar = "lambda")
points(ylab="number of predictors",x=log(lassoMod$lambda),y=sapply(1:ncol(lassoMod$beta),function(x) sum(lassoMod$beta[,x]!=0)),type="line",col="blue")
idx=c(1,8,10,25,64,81,length(lassoMod$lambda))
idx
abline(v=log(lassoMod$lambda[idx]))
lassoMod_steps <- glmnet(lambda = lassoMod$lambda[idx],x = x, y = y, alpha = 1, family = "binomial")
lassoMod_steps_deviance=(1-lassoMod_steps$dev.ratio)*lassoMod$nulldev
lassoMod_steps_deviance
plot(ylab = "log deviance",xlab="number of predictors",x=c(1:7),y=log(lassoMod_steps_deviance),type="line")
text(x = 1:7,y = log(lassoMod_steps_deviance),labels = round(lassoMod_steps_deviance,0),pos=c(4,3,3,4,3,3,3))
#From this result we can see that a model with just 
#5 predictors will have the same deviance as another
#applying all the predictors, so there are some variables
#that can be removed using this approach
########################## 
plot(lassoMod,label = TRUE, xvar = "lambda")
plot(y=lassoMod$dev.ratio,x=log(lassoMod$lambda))

plot(ridgeMod, label = TRUE, xvar = "dev")
plot(lassoMod, label = TRUE, xvar = "dev")

set.seed(12345)
ncvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10, family = "binomial")
plot(ncvLasso)
kcvLasso$lambda.min

set.seed(12345)
ncvRidge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10, family = "binomial")
plot(ncvRidge)
ncvRidge$lambda.min

predict(ncvLasso, type = "coefficients", s = ncvLasso$lambda.1se)
predict(ncvRidge, type = "coefficients", s = ncvRidge$lambda.1se)

##############################Logistic Regression By attribute########################

diff_model<-function(response,predictor,name="",data=admision){
  predictor_scaled=scale(predictor)
  mod=glm(response>cutoff~predictor_scaled,family = "binomial",data=admision)
  x_plot=seq(-3,3,by=0.1)
  temp=mod$coefficients[1]+mod$coefficients[2]*x_plot
  x_plot=x_plot*sd(predictor)+mean(predictor)
  plot(x=x_plot,y=logistic(temp),type = "line",xlab=name,ylab = "Probability")
  points(x=predictor,y=response>cutoff)
  x_d=(-mod$coefficients[1]/mod$coefficients[2])*sd(predictor)+mean(predictor)
  y_d=0.5
  points(x_d,y_d,pch=19,col="blue")
  text(x_d,y_d,labels = round(x_d,2),pos=4)
  tab=table(Chance>cutoff,mod$fitted.values>0.5)
  print(tab)
  accuracy <- sum(diag(tab)) / sum(tab)
  print(paste("Accuracy:",accuracy))
}

diff_model(Chance,GRE,"GRE")
lines(GRE, Chance, type = "plot.default")
diff_model(Chance,TOEFL,"TOEFL")
diff_model(Chance,UniRating,"UniRating")
diff_model(Chance,SOP,"SOP")
diff_model(Chance,LOR,"LOR")
diff_model(Chance,CGPA,"CGPA")

###################Para hacer pruebas#####################
predictor_scaled=scale(GRE)
mod=glm(Chance>cutoff~predictor_scaled,family = "binomial",data=admision)
summary(mod$fitted.values>0.5)
x_plot=seq(-3,3,by=0.1)
temp=mod$coefficients[1]+mod$coefficients[2]*x_plot
x_plot=x_plot*sd(GRE)+mean(GRE)
plot(x=x_plot,y=logistic(temp),type = "line")
points(x=GRE,y=Chance>cutoff)
x_d=(-mod$coefficients[1]/mod$coefficients[2])*sd(GRE)+mean(GRE)
y_d=0.5
points(x_d,y_d,pch=19,col="blue")
text(x_d,y_d,labels = round(x_d,2),pos=4)
tab=table(Chance>cutoff,mod$fitted.values>0.5)
print(tab)




mod_1=glm(Chance>cutoff~GRE,family = "binomial",data=admision)
mod_2=glm(Chance>cutoff~GRE_norm,family = "binomial",data=admision)
logistic(predict(mod_1,admision[2,]))
logistic(predict(mod_2,admision_norm[2,]))





#####################APPLYING LOGISTIC REGRESSION MODEL###########
Anova(lm(Chance~.,data = admision))
# By using the ANOVA table for a linear model of the whole dataset against the chance
# we can be see that the most important predictors are GRE, TOEFL, LOR, CGPA and Research
# ?This will be taken into account, trying to avoid very simple model due to 
# high correlated values shown before. ?

### Let's build one model for each predictor
plot(ecdf(Chance))
plot(density(Chance))




#######################################################################################
########### I wouldn't normalize/standardize the data as it does not make a difference
########### in the models...
## Standardize the data 
#admision_norm=admision
#admision_norm[,-7]=sapply(admision_norm[,-7],scale)
#names(admision_norm)
#names(admision_norm)=c(paste(names(admision_norm),"_norm",sep=""))
#attach(admision_norm)




## POISSON
##mod_poi <- stepAIC(glm(UniRating ~ ., data=admision, family = "poisson"), k = log(length(Chance)))
##summary(mod_poi)


## GRE + LOR + CGPA
#AIC with all predictors works well - Research seems to have high impact!
##mod<-stepAIC(glm(Chance > cutoff ~ ., data = admision, family = "binomial"), k = 2)
#BIC with all predictors not scaled performs better than AIC? - let's check later
### The result is the same taking into account normalized data (Same AIC, same Null Deviance and same Residual Deviance)
#Let's work with normalized data 
#mod_norm <- stepAIC(glm(Chance_norm > cutoff_norm ~ ., data = admision_norm, family = "binomial"), k = log(length(Chance)))
## GRE_norm + LOR_norm + CGPA_norm
#stepAIC(glm(Chance > cutoff ~ ., data = admision_norm, family = "binomial"), k = log(length(Chance)))
