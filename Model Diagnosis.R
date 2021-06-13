setwd("C:/Users/shiva/OneDrive/Desktop/R working directory/MODULE-6")

#Writing state:X77 dataset as states, Build in Dataset of R
states<- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
head(states)

#---------------------------------------------------------------------------#

#Correlation between Variables
cor(states)

#Scatterplot Matrix 
library(car)
scatterplotMatrix(states,spread=FALSE,smoother.args=list(lty=2),main="Scatterplot Matrix(States)")

#Scatterplot & Correlation
library(psych)
pairs.panels(states)

#----------------------------------------------------------------------------
#Defining Fit:MultiLinear Regression Model

Fit<- lm(Murder~Population+Illiteracy+Income+Frost,data = states)

summary(Fit)
confint(Fit)

#-------------------------------------------------------------------------------
#Plot the Regression Summary
par(mfrow=c(2,2))
plot(Fit)

graphics.off()

#------------------------------------------------------------------------------
#Normality ti understand the residuals distribution
#library(car)

qqPlot(Fit,labels=row.names(states),id.method="identify",simulate = TRUE,main="Q-Q-Plot")

#-----------------------------------------------------------------------------
#Component + Residual Plot (Linearity)

crPlots(Fit)

#Homosecdasticity (NON CONSTANT ERROR VARIENCE)
ncvTest(Fit)
spreadLevelPlot(Fit)


#---------------------------------------------------------------------------
#Global Test of Linear Model assumptions
library(gvlma)
gvmodel<- gvlma(Fit)
gvmodel

summary(gvmodel)

#-------------------------------------------------------------------------
#Evaluation Multicollinearity
#Library(car)

vif(Fit)
sqrt(vif(Fit))>2

#------------------------------------------------------------------
#Identifying outliers/Influenctial obs.
#library(car)

outlierTest(Fit)

#Added-Variable plots
#library(car)

avPlots(Fit,ask=FALSE,id.method="identify")

#Influential obs
influencePlot(Fit,id.method="identify",main="Influence Plot",sub="Circle Size is proportional to cook's distance")

#----------------------------------------------------------------------
#Model comparison
#comparing models using anova function
fit1<- lm(Murder~Population+Illiteracy+Income+Frost,data = states)
fit2<- lm(Murder~Population+Illiteracy,data = states)

anova(fit1,fit2)

#---------------------------------------------------------------------------
#Comparing models with AIC 
fit1<- lm(Murder~Population+Illiteracy+Income+Frost,data = states)
fit2<- lm(Murder~Population+Illiteracy,data = states)

AIC(fit1,fit2)

#----------------------------------------------------------------------------
#Backward Stepwise selection
library(MASS)
attach(states)

fit<- lm(Murder~Population+Illiteracy+Income+Frost,data = states)
stepAIC(fit,direction = "backward")

#------------------------------------------------------------------------------
#All Subset regression
library(leaps)
attach(states)

leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost,data = states,nbest = 4)
leaps

plot(leaps,scale = "adjr2",main = "Selecting the best regression")

#library(car)
subsets(leaps,statistic="cp",main="Cp plot for all subsets regression")
abline(1,1,lty=2,col="red")
