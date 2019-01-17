################################INITIALIZATION############################
setwd("/Users/Jaime/Desktop/Master/PredictiveModeling/Project2/")



##################
>>>>>>> 01b96fcfe96b7fadd0becf6e1508710965884165
admision = read.csv("Admission_Predict.csv",header = TRUE)
summary(admision)
# Get rid of serial number since it doesn't really mean anything
admision=admision[,c(-1)]
options(warn=-1)
library("psych")
library("MASS")
library("scatterplot3d")
library("pracma")
library("tidyverse")
library("car")
library("glmnet")

#The parameters included are : 
##1. GRE Scores (out of 340) GRE Continua
##2. TOEFL Scores (out of 120) TOEFL Continua
##3. University Rating (out of 5) UniRating Discreta
##4. Statement of Purpose (out of 5) SOP Discreat
##5. Letter of Recommendation Strength (out of 5) LOR Discreat
##6. Undergraduate GPA (out of 10) CGPA Continu
##7. Research Experience (either 0 or 1) Research Binaria 
##8. Chance of Admit (ranging from 0 to 1) Chance Continua
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
#From this plot we can see that variables are highly correlated

#####################FIXING VARIABLES#######################
 
summary(admision)
#There are no outliers nor NA's that need to be fixed
admision$Research=as.factor(admision$Research)
#fixing research that is binary variable
## Boxplot for each variable scaled
boxplot(sapply(admision[,-7],scale))

## Standardize the data
admision_norm=admision
admision_norm[,-7]=sapply(admision_norm[,-7],scale)
names(admision_norm)
names(admision_norm)=c(paste(names(admision_norm),"_norm",sep=""))
attach(admision_norm)

#####################APPLYING LOGISTIC REGRESSION MODEL###########
Anova(lm(Chance~.,data = admision))
# By using the ANOVA table for a linear model of the whole dataset against the chance
# we can be see that the most important predictors are GRE, TOEFL, LOR, CGPA and Research
# ?This will be taken into account, trying to avoid very simple model due to 
# high correlated values shown before. ?

### Let's build one model for each predictor
plot(ecdf(Chance))
plot(density(Chance))
abline(v=0.9)
summary(Chance > 0.9)
# Here we can see, as we already know, that chance does not have 
# a mean equal to 0.5. So let's try other approach for our problem
cutoff=0.7
cutoff_norm = (cutoff-mean(Chance))/std(Chance)
summary(Chance>cutoff)
summary(Chance_norm>cutoff_norm)

mod <- stepAIC(glm(Chance > cutoff ~ ., data = admision, family = "binomial"), k = log(length(Chance)))
summary(mod)
pairs.panels(admision[,c(1,5,6,8)], 
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

plot(Chance ~ GRE, data = admision, col=LOR)
plot(Chance ~ CGPA, data = admision, col=LOR)

## Allowing for interactions returns the same result -> The important predictors are GRE, LOR and CGPA
mod_int <- stepAIC(glm(Chance > cutoff ~ .^2, data=admision, family="binomial"), k = log(length(Chance)))
summary(mod_int)


## POISSON
mod_poi <- stepAIC(glm(UniRating ~ ., data=admision, family = "poisson"), k = log(length(Chance)))
summary(mod_poi)


## GRE + LOR + CGPA
#AIC with all predictors works well - Research seems to have high impact!
stepAIC(glm(Chance > cutoff ~ ., data = admision, family = "binomial"), k = 2)
#BIC with all predictors not scaled performs better than AIC? - let's check later
### The result is the same taking into account normalized data (Same AIC, same Null Deviance and same Residual Deviance)
#Let's work with normalized data 
mod_norm <- stepAIC(glm(Chance_norm > cutoff_norm ~ ., data = admision_norm, family = "binomial"), k = log(length(Chance)))
## GRE_norm + LOR_norm + CGPA_norm
#stepAIC(glm(Chance > cutoff ~ ., data = admision_norm, family = "binomial"), k = log(length(Chance)))

summary(mod)
plot(no2Real$particles>180 ~ tempDiff25to2)#, xlim = c(-100, 350))
x <- seq(6, 11, l = 2000)
y <- exp(-(mod$coefficients[1] + mod$coefficients[2] * x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)


#Let's check the performance of this model:
val=logistic(x=predict(mod,admision_norm))
tab <- table(Chance > cutoff, val > 0.5)
tab
accuracy <- sum(diag(tab)) / sum(tab)
accuracy

## Interpretations for the model (regarding the odds:
#Taking into account that we are currently on January
#and we have enough time to prepare well just one exam, 
#which one should be the best option - check which are the higher B's


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
  #return(mod)
  
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

