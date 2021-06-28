#setting Working directory
setwd("C:/Users/shiva/OneDrive/Desktop/R working directory/MODULE-6")

#Importing Data into R
df<- read.csv("C:/Users/shiva/Downloads/bodyfat-reduced (1).csv")
options(scipen = 0)
head(df)
dim(df)
library(psych)
library(GPArotation)

#Extracting Correlation matrix out of data
df_cor<-cor(df)
df_cor
#---------------------------------------------------------
#To find number of factors required for analysis

fa.parallel(df_cor,fa = "both",n.iter = 100)


#-------------------------------------------------------
#To find the principle axis: unrotated
FA<- fa(df_cor,nfactors=2,rotate="none",fm="pa")
FA

#----------------------------------------------------
#To find the principle axis:orthogonal rotation(Factors unrelated)

FA.varimax<- fa(df_cor,nfactors=2,rotate="varimax",fm="pa")
FA.varimax

fa.diagram(FA.varimax) # #Plot the factor diagram
#----------------
#To find the principle axis: oblique rotation(Factors related)
FA.promax<- fa(df_cor,nfactors=2,rotate="promax",fm="pa")

fa.diagram(FA.promax)

#we find there is hight correlation between 2 factors
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

#Factor Analysis
FA<- factanal(df,factors = 2,rotation = 'varimax')
FA
attributes(FA)
FA$loadings
FA$converged
FA$factors

##All variables are heavy on Factor1 except BodyFat.
##In Factor 2 BodyFat,Weight,Chest,Abdomen,Hip
#are heavy


#correlation btw variables
FA$correlation
summary(FA)
FA1<- factanal(df,factors = T)
FA1
##we can see all variables are heavy on Factor2

#we can see that appropriate number of factors is 1,Since it has most significance.

