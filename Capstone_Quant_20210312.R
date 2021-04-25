#Data Science Capstone Project
#Merrimack College - Winter 2021
#Severity Estimation

#Invoke the required library packages
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(ISLR)
library(MASS)
library(caret)
library(glmnet)
library(pls)
library(plotrix)

set.seed(324) 
setwd("D:/Merrimack/Capstone Project/R Work - Analysis/")

DSFinalMerge <- read.csv("D:/Merrimack/Capstone Project/Data/DSFinalMerge_New312.csv")
DSFinalMerge_drops <- c("X","FilingName","SettleYN")
DSFinalMerge <- DSFinalMerge[ , !(names(DSFinalMerge) %in% DSFinalMerge_drops)]
#source("D:/Merrimack/Capstone Project/R Work - Data Cleansing/Combine_New_202010222.R")
quant1 <- DSFinalMerge
quant1 <- quant1[,c(66,1:65)]
#quant1_drops <- c("loc","gsector","spcindcd","spcseccd","spsticrm","idbflag","dvrate")
quant1_drops <- c("gsector","idbflag")
quant1 <- quant1[ , !(names(quant1) %in% quant1_drops)]

########## ########## FEATURE ENGINEERING ########## ##########

quant1$costat <- ifelse(quant1$costat == "A",0,1)
quant1$loc <- ifelse(quant1$loc == "USA",0,1)

#Convert state to numbers
#unique(quant1$state)
quant1$state <- case_when(
quant1$state == "AB" ~ 1,
quant1$state == "AL" ~ 2,
quant1$state == "AR" ~ 3,
quant1$state == "AZ" ~ 4,
quant1$state == "BC" ~ 5,
quant1$state == "CA" ~ 6,
quant1$state == "CO" ~ 7,
quant1$state == "CT" ~ 8,
quant1$state == "DC" ~ 9,
quant1$state == "DE" ~ 10,
quant1$state == "FL" ~ 11,
quant1$state == "GA" ~ 12,
quant1$state == "HI" ~ 13,
quant1$state == "IA" ~ 14,
quant1$state == "ID" ~ 15,
quant1$state == "IL" ~ 16,
quant1$state == "IN" ~ 17,
quant1$state == "KS" ~ 18,
quant1$state == "KY" ~ 19,
quant1$state == "LA" ~ 20,
quant1$state == "MA" ~ 21,
quant1$state == "MB" ~ 22,
quant1$state == "MD" ~ 23,
quant1$state == "MI" ~ 24,
quant1$state == "MN" ~ 25,
quant1$state == "MO" ~ 26,
quant1$state == "MS" ~ 27,
quant1$state == "MT" ~ 28,
quant1$state == "NB" ~ 29,
quant1$state == "NC" ~ 30,
quant1$state == "NE" ~ 31,
quant1$state == "NF" ~ 32,
quant1$state == "NH" ~ 33,
quant1$state == "NJ" ~ 34,
quant1$state == "NM" ~ 35,
quant1$state == "NS" ~ 36,
quant1$state == "NV" ~ 37,
quant1$state == "NY" ~ 38,
quant1$state == "OH" ~ 39,
quant1$state == "OK" ~ 40,
quant1$state == "ON" ~ 41,
quant1$state == "OR" ~ 42,
quant1$state == "PA" ~ 43,
quant1$state == "PR" ~ 44,
quant1$state == "QC" ~ 45,
quant1$state == "RI" ~ 46,
quant1$state == "SC" ~ 47,
quant1$state == "SD" ~ 48,
quant1$state == "TN" ~ 49,
quant1$state == "TX" ~ 50,
quant1$state == "UT" ~ 51,
quant1$state == "VA" ~ 52,
quant1$state == "WA" ~ 53,
quant1$state == "WI" ~ 54,
quant1$state == "WV" ~ 55,
quant1$state == "WY" ~ 56,
quant1$state == "ZZ" ~ 57)

#Convert acctstd to numbers
#unique(quant1$acctstd)
quant1$acctstd <- case_when(
  quant1$acctstd == "DI" ~ 1,
  quant1$acctstd == "DS" ~ 2,
  quant1$acctstd == "DU" ~ 3,
  quant1$acctstd == "ZZ" ~ 4
)

colnames(quant1)[colSums(is.na(quant1)) > 0]

quant1 <- quant1 %>% mutate(liqratio= act/lct)
quant1$liqratio <- ifelse(quant1$liqratio == "NaN"|quant1$liqratio == "Inf",0,quant1$liqratio)

quant1 <- quant1 %>% mutate(dtaratio= dltt/at)
quant1$dtaratio <- ifelse(quant1$dtaratio == "NaN"|quant1$dtaratio == "Inf" ,0,quant1$dtaratio)

#Make a dataframe that contains only the target row for this class.
#My gvkey is 25119 for Mohawk Inc
MHK <- quant1 %>% filter(gvkey == 25119)
#later you're going to scale this df

#NACounts <- as.data.frame(colSums(is.na(quant1)))

#examine the cleansed data set before removing outliers etc
quantLM_all <- lm(SettleAmount ~., data = quant1)
summary(quantLM_all)
sm <- summary(quantLM_all)
MSE <-  mean(sm$residuals^2)
MSE
par(mfrow=c(2, 2))
plot(quantLM_all, main = "Initial Cleansed Data Set")
#par(mfrow=c(1, 2))

#========= DEAL WITH OUTLIERS =========#
#Break the data into two sets. One with settlements and one without
SttlRows <- quant1 %>% filter(SettleAmount > 0)
NoSttlRows <- quant1 %>% filter(SettleAmount == 0)
#NoSttlRows <- sample_n(NoSttlRows, 500, replace = FALSE)

#Remove outliers from data set with no Settlement rows
outliers <- function(x) {
  Q1 <- quantile(x, probs=.15)
  Q3 <- quantile(x, probs=.85)
  iqr = Q3-Q1
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
NoSttlRows <- remove_outliers(NoSttlRows,)
#merge the data back together. 
#There will be significantly fewer rows
quant3 <- rbind(SttlRows, NoSttlRows)
quant1 <- quant3

#colnames(quant1)[colSums(is.na(quant1)) > 0]
#NACounts <- as.data.frame(colSums(is.na(quant1)))

#Check the new data set for normality issues
quantLM_all <- lm(SettleAmount ~.,data = quant1)
summary(quantLM_all)
sm <- summary(quantLM_all)
MSE <-  mean(sm$residuals^2)
MSE
par(mfrow=c(2, 2))
plot(quantLM_all, main = "Outliers Removed Data Set")
#par(mfrow=c(1, 2))

#Chart to show skew of response variable
# par(mfrow=c(1, 1))
# plot(density(quant1$SettleAmount), main="Density Plot: Schedule Deviation", ylab="Frequency"
#      , sub=paste("Skewness:", round(e1071::skewness(quant1$SettleAmount), 2))) 
# polygon(density(quant1$SettleAmount), col="lavender")
# 
# #Transform
# quantLM_all <- lm(sqrt(SettleAmount) ~.,data = quant1)
# summary(quantLM_all)
# sm <- summary(quantLM_all)
# MSE <-  mean(sm$residuals^2)
# MSE
# par(mfrow=c(2, 2))
# plot(quantLM_all)
# par(mfrow=c(1, 2))


#Create the training and test sets
training_ind <- createDataPartition(quant1$SettleAmount, p = 0.7, list = FALSE, times = 1)
train <- quant1[training_ind, ] 
test <- quant1[-training_ind, ]

#Scale the training and test sets and the MHK df

QTtestscl <- as.data.frame(scale(test,
                          center = apply(train, 2, mean),
                          scale = apply(train, 2, sd)))

QTtrainscl <- as.data.frame(scale(train))

QTMHKscl <- as.data.frame(scale(MHK,
                          center = apply(train, 2, mean),
                          scale = apply(train, 2, sd)))

#Unscale by hand
# scale, with default settings, will calculate the mean and standard deviation of the entire vector, 
# then "scale" each element by those values by subtracting the mean and dividing by the sd. 
# (If you use scale(x, scale=FALSE), it will only subtract the mean but not divide by the std deviation.)

trainmean <- mean(train$SettleAmount)
trainsd <- sd(train$SettleAmount)
#Prove it to yourself
# trainmean
# trainsd
# scaledresponse <- (MHK$SettleAmount-trainmean)/trainsd
# unscaledresponse <- (scaledresponse*trainsd)+trainmean

#Linear Regression with Train/Test
lr_trn <- lm(SettleAmount ~
               dlrsn
             + capx
             + ni
             + mkvalt
             + ivst
                 , data = QTtrainscl)
summary(lr_trn)
sm <- summary(lr_trn)
MSE.lr.trn <-  mean(sm$residuals^2)
MSE.lr.trn

pred.lr.test <- predict(lr_trn, QTtestscl)
MSE.lr.test <- mean((pred.lr.test-QTtestscl$SettleAmount)^2)
MSE.lr.test

pred.lr.MHK <- predict(lr_trn, QTMHKscl)
pred.lr.MHK
(pred.lr.MHK*trainsd)+trainmean
((pred.lr.MHK*trainsd)+trainmean)+1.96*trainsd
((pred.lr.MHK*trainsd)+trainmean)-1.96*trainsd

coef(lr_trn)

RespFormula <- 2.356763E-16 + 
  QTMHKscl$dlrsn*0.16127 + 
  QTMHKscl$capx*-3.909607 + 
  QTMHKscl$ni*-12.91534 + 
  QTMHKscl$mkvalt*1.117674 + 
  QTMHKscl$ivst*17.22299 
RespFormula 

unspred.linrMHK <- (RespFormula*trainsd)+trainmean
CI.linrMHK_high <- unspred.linrMHK+1.96*trainsd
CI.linrMHK_low <- unspred.linrMHK-1.96*trainsd
CI.linrMHK_high
CI.linrMHK_low

##### RIDGE
#Convert the scaled train/test sets to matrices
set.seed(324)
trainscl.mat =model.matrix(SettleAmount ~
                             cshtrm
                           +splticrm
                           +prclm
                           +mkvalt
                           , data=QTtrainscl)
testscl.mat =model.matrix(SettleAmount ~
                            cshtrm
                          +splticrm
                          +prclm
                          +mkvalt
                          , data=QTtestscl)
MHKscl.mat =model.matrix(SettleAmount ~
                           cshtrm
                         +splticrm
                         +prclm
                         +mkvalt
                          , data=QTMHKscl)

#Use cross-validation glmnet to choose the best lambda
grid = 10 ^seq(4, -2, length=100)
mod.ridge =cv.glmnet(trainscl.mat, QTtrainscl[, "SettleAmount"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best

#Put the lambda estimate into the ridge regression model
ridge.pred =predict(mod.ridge, newx=testscl.mat, s=lambda.best)
MSE.ridge <- mean((QTtestscl[, "SettleAmount"] - ridge.pred)^2)
MSE.ridge

#Run the model against the single MHK row to get the predicted value
# ridge.predMHK =predict(mod.ridge, newx=MHKscl.mat, s=lambda.best)
# MSE.ridgeMHK <- mean((QTMHKscl[, "SettleAmount"] - ridge.pred)^2)
# MSE.ridgeMHK


coef(mod.ridge)

ridge.respformula <- -1.494035e-16 + 
  QTMHKscl$cshtrm * 7.533586e-02 + 
  QTMHKscl$splticrm * 7.087897e-02 + 
  QTMHKscl$prclm * 6.359994e-02 + 
  QTMHKscl$mkvalt * 5.789613e-02
ridge.respformula 
#This produces 0.9098228


#The hand written formula produces the same prediction as running the single row
#MHK data set against the model. Awesome.
round(ridge.predMHK,6) == round(ridge.respformula,6)
unspred.ridgeMHK <- (ridge.predMHK*trainsd)+trainmean
unspred.ridgeMHK
  
#Now unscale the predicted value and create the confidence interval
unspred.ridgeMHK <- (ridge.respformula*trainsd)+trainmean
CI.ridgeMHK_high <- unspred.ridgeMHK +1.96*trainsd
CI.ridgeMHK_low <- unspred.ridgeMHK -1.96*trainsd
unspred.ridgeMHK
CI.ridgeMHK_high
CI.ridgeMHK_low

##### LASSO
#install.packages("mvtnorm")
#library("mvtnorm") 

#Convert the scaled train/test sets to matrices
set.seed(324)
trainscl.mat =model.matrix(SettleAmount ~
                             dlrsn
                           + capx
                           + ni
                           + mkvalt
                           , data=QTtrainscl)
testscl.mat =model.matrix(SettleAmount ~
                            dlrsn
                          + capx
                          + ni
                          + mkvalt
                          , data=QTtestscl)
MHKscl.mat =model.matrix(SettleAmount ~
                           dlrsn
                         + capx
                         + ni
                         + mkvalt
                         , data=QTMHKscl)

grid = 10^seq(10, -2, length = 100)
fit.lasso <- glmnet(trainscl.mat, QTtrainscl$SettleAmount
                    , alpha = 1
                    , lambda = grid
                    , thresh = 1e-12)

cv.lasso <- cv.glmnet(trainscl.mat, QTtrainscl$SettleAmount
                      , alpha = 1
                      , lambda = grid
                      , thresh = 1e-12)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

lasso.pred =predict(fit.lasso, s=bestlam.lasso, newx=testscl.mat)
MSE.lasso <- mean((QTtestscl[, "SettleAmount"] - lasso.pred)^2)
MSE.lasso

#Get the lasso model coefficients
lasso.coeffs <- predict(fit.lasso, s=bestlam.lasso, type="coefficients")
lasso.coeffs<- data.frame( predict_names = rownames(lasso.coeffs),
                           coef_vals = matrix(lasso.coeffs))

lasso.coeffs

lasso.respformula <- -1.043737e-16 +
  QTMHKscl$dlrsn*0 +
  QTMHKscl$capx*2.588570e-01 +
  QTMHKscl$ni*0 +
  QTMHKscl$mkvalt*4.205302e-01
lasso.respformula

unspred.lassoMHK <- (lasso.respformula*trainsd)+trainmean
CI.lassoMHK_high <- unspred.lassoMHK+1.96*trainsd
CI.lassoMHK_low <- unspred.lassoMHK-1.96*trainsd
unspred.lassoMHK
CI.lassoMHK_high
CI.lassoMHK_low

#Check to make sure the prediction match 
# lasso.predMHKc =predict(fit.lasso, newx=MHKscl.mat, s=bestlam.lasso)
# MSE.lassoMHKc <- mean((QTMHKscl[, "SettleAmount"] - lasso.predMHKc)^2)
# MSE.lassoMHKc
# 
# unspred.lassoMHKc <- (lasso.predMHKc*trainsd)+trainmean
# CI.lassoMHK_highc <- unspred.lassoMHKc+1.96*trainsd
# CI.lassoMHK_lowc <- unspred.lassoMHKc-1.96*trainsd
# unspred.lassoMHKc
# CI.lassoMHK_highc
# CI.lassoMHK_lowc


#####---Compare All Model MSEs 
##============================================================================
Model <- c("Linear MSE"
           ,"Ridge MSE"
           ,"Lasso MSE")
MSE <-c(MSE.lr.trn
        ,MSE.ridge
        ,MSE.lasso)
MSEs <- cbind(Model,MSE)
MSEs

par(mfrow=c(1, 1))
par(mar=c(8, 4, 4, 2) + 0.1)
bp<-barplot(c(MSE.lr.trn
              ,MSE.ridge
              ,MSE.lasso),
            col = "lightblue", names.arg = c("Linear MSE"
                                             ,"Ridge MSE"
                                             ,"Lasso MSE"),
            main = "MSEs of Severity Estimation Models",las=1)
text(bp, 0, format(round(c(MSE.lr.trn
                    ,MSE.ridge
                    ,MSE.lasso),4),big.mark=",", trim=TRUE),cex=1,pos=3)


##======== Compare All Model MHK Unscaled Prediction Values
##============================================================================
Model <- c("Linear Regression"
           ,"Ridge Regression"
           ,"Lasso Regression")
Pred <-c(unspred.linrMHK
        ,unspred.ridgeMHK
        ,unspred.lassoMHK)
Preds <- as.data.frame(cbind(Model,Pred))
Preds$Pred <- as.numeric(Preds$Pred)

PredRange <- 0:max(Preds$Pred)
pts <- pretty(PredRange)

par(mfrow=c(1, 1))
par(mar=c(8, 8, 4, 2) + 0.1)
bp<-barplot(c(unspred.linrMHK
              ,unspred.ridgeMHK
              ,unspred.lassoMHK),
            col = "lavender", names.arg = c("Linear Regression"
                                            ,"Ridge Regression"
                                            ,"Lasso Regression"),
            main = "Settlement Severity Predictions",las=1,ylab ="",axes = F)
title(ylab="US Dollars", line=5.5)
text(bp, 0, format(round(c(unspred.linrMHK
                    ,unspred.ridgeMHK
                    ,unspred.lassoMHK),0),big.mark=",", trim=TRUE),cex=1,pos=3)
axis(2,at = pts, labels=format(pts, scientific=F, big.mark=","), las = 1)


##======== Compare All Model MHK Confidence Intervals
##============================================================================

upperlimit = c(CI.linrMHK_high
               ,CI.ridgeMHK_high
               ,CI.lassoMHK_high)
lowerlimit = c(CI.linrMHK_low
               ,CI.ridgeMHK_low
               ,CI.lassoMHK_low)

mean = c(unspred.linrMHK
         ,unspred.ridgeMHK
         ,unspred.lassoMHK)

names =  c("Linear Regression"
          ,"Ridge Regression"
          ,"Lasso Regression")

namevec <- as.vector(names)

df = data.frame(cbind(upperlimit,lowerlimit,mean))
CIrange <- pretty(max(df$upperlimit):min(df$lowerlimit))

par(mfrow=c(1, 1))
par(mar=c(6, 6, 1, 2)+2 )
cip<-plot(df$mean
     , ylim = c(-500000,40000000)

     ,main="Settlement Severity Predictions 95% Confidence Intervals"
     ,axes = F
     ,xlab = "Model"
     ,ylab = ""
     )
#require(plotrix)
plotCI(df$mean
             ,y=NULL
             , uiw=df$upperlimit-df$mean
             , liw=df$mean-df$lowerlimit
             #, err="y"
             , pch=20
             , slty=3
             , col="red"
             , scol = "blue"
             #, gap=0.02
             , add=TRUE
       ,axes = F
       )
pts <- pretty(CIrange)
title(ylab="US Dollars", line=5.5)
axis(1, at=1:3, labels=namevec) 
axis(2,at = pts, labels=format(pts,scientific=F,big.mark=",", trim=TRUE), las = 1)



FN <- read.csv("D:/Merrimack/Capstone Project/Data/DSFinalMerge_New312.csv")
SttlRows <- FN %>% filter(SettleAmount > 0)
SttlRows <- subset(SttlRows, select = c(FilingName, gvkey, ggroup, gind, gsector,gsubind, sic, SettleAmount))
SttlRows %>% filter(gind == '252010')  %>%
          mutate(avg = mean(SettleAmount))
