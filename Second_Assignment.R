################################SET WORKING DIR###########################
setwd("/Users/Jaime/Desktop/Master/PredictiveModeling/Project2/")


################################IMPORTING LIBRARIES#######################
library("psych")
library("MASS")
library("scatterplot3d")
library("pracma")
library("tidyverse")
library("car")
library("glmnet")



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



############################EXPLORATORY ANALYSIS#####################
pairs.panels(admision, 
             method = "pearson", # correlation method
             hist.col = "#00146E",
             col = "red",
             lm = FALSE,
             ellipses = FALSE,
             smooth = FALSE,
             #pch = c(21,21)[class],
             #bg=my_cols[class],
             rug = FALSE,
             cex.cor = 5,
             scale = TRUE,
             density = TRUE  # show density plots
)
# All variables but Research follow a Gaussian distribution
# The most correlated variables against Chance (which is the target variable) are GRE
# and CGPA which could be understood as equivalent to the PAU in Spain

# The parameters included are : 
## 1. GRE Scores (out of 340) GRE Quantitative variable
summary(GRE)
boxplot(GRE)
plot(ecdf(GRE))
plot(density(GRE))
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
# Does'nt make sense in qualitative variables as much as in continuous.
plot(ecdf(UniRating))
plot(density(UniRating))
## No outliers for UniRating and as seen in the first plot it follows a Normal distribution

## 4. Statement of Purpose (out of 5) SOP Quantitative variable that measures the goodnes of
## the statement of purpose
summary(SOP)
boxplot(SOP)
# Does'nt make sense in qualitative variables as much as in continuous.
plot(ecdf(SOP))
plot(density(SOP))
## No outliers for SOP and as seen in the first plot it follows a Normal distribution

## 5. Letter of Recommendation Strength (out of 5) LOR Quantitative variable (Very similar to 
## the previous one but concerning letters of recommendation)
summary(LOR)
boxplot(LOR)
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
diff_model(Chance,UniRating,"UniRating", gr=TRUE)
diff_model(Chance,SOP,"SOP", gr=TRUE)
diff_model(Chance,LOR,"LOR", gr=TRUE)
diff_model(Chance,CGPA,"CGPA", gr=TRUE)
#diff_model(Chance,Research,"Research", gr=TRUE)
# As we can see we can be more certain that the classifier has returned a correct answer
# when it says that we will not me admitted because the TNR is bigger than the TPR.
# There variables have the produce best TPR's are CGPA, GRE and TOEFL.
# We could also see what happens when using polynomial predictors

##diff_model(Chance,Research+GRE,"aa")
for (i in 1:(length(admision)-1)) {
  for (j in 1:(length(admision)-1)) {
    if (j > i) {
      diff_model(Chance,admision[i]+admision[j], paste(names(admision)[i],"+",names(admision)[j]))
    }
  }
}


## BEST MODEL ACCORDING TO BIC
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

## Interpretations for the model (regarding the odds:
# Taking into account that we are currently on January
# and we have enough time to prepare well just one exam, 
# which one should be the best option - check which are the higher B's












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
