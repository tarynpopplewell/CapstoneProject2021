#Data Science Capstone Project
#Merrimack College - Winter 2021
#Likelihood Estimation

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
library(leaps)
library(class)
library(ROCR)

set.seed(324) 
setwd("D:/Merrimack/Capstone Project/R Work - Analysis/")

#DS_QL <- read.csv("D:/Merrimack/Capstone Project/Data/DS_QL.csv")
DS_QL <- read.csv("D:/Merrimack/Capstone Project/Data/DSFinalMerge_New312.csv")
DS_QL <- subset(DS_QL,select = -c(X,FilingName))
#source("D:/Merrimack/Capstone Project/R Work - Data Cleansing/Combine_New_20210304.R")
qual1 <- DS_QL
qual1 <- qual1[,c(67,1:65)]
#These cause trouble down the road so remove them here.
qual1 <- subset(qual1,select = -c(idbflag,gsector,spsticrm,dvrate))

########## ########## FEATURE ENGINEERING ########## ##########

qual1$costat <- ifelse(qual1$costat == "A",0,1)
qual1$loc <- ifelse(qual1$loc == "USA",0,1)

#Convert state to numbers
#unique(qual1$state)
qual1$state <- case_when(
  qual1$state == "AB" ~ 1,
  qual1$state == "AL" ~ 2,
  qual1$state == "AR" ~ 3,
  qual1$state == "AZ" ~ 4,
  qual1$state == "BC" ~ 5,
  qual1$state == "CA" ~ 6,
  qual1$state == "CO" ~ 7,
  qual1$state == "CT" ~ 8,
  qual1$state == "DC" ~ 9,
  qual1$state == "DE" ~ 10,
  qual1$state == "FL" ~ 11,
  qual1$state == "GA" ~ 12,
  qual1$state == "HI" ~ 13,
  qual1$state == "IA" ~ 14,
  qual1$state == "ID" ~ 15,
  qual1$state == "IL" ~ 16,
  qual1$state == "IN" ~ 17,
  qual1$state == "KS" ~ 18,
  qual1$state == "KY" ~ 19,
  qual1$state == "LA" ~ 20,
  qual1$state == "MA" ~ 21,
  qual1$state == "MB" ~ 22,
  qual1$state == "MD" ~ 23,
  qual1$state == "MI" ~ 24,
  qual1$state == "MN" ~ 25,
  qual1$state == "MO" ~ 26,
  qual1$state == "MS" ~ 27,
  qual1$state == "MT" ~ 28,
  qual1$state == "NB" ~ 29,
  qual1$state == "NC" ~ 30,
  qual1$state == "NE" ~ 31,
  qual1$state == "NF" ~ 32,
  qual1$state == "NH" ~ 33,
  qual1$state == "NJ" ~ 34,
  qual1$state == "NM" ~ 35,
  qual1$state == "NS" ~ 36,
  qual1$state == "NV" ~ 37,
  qual1$state == "NY" ~ 38,
  qual1$state == "OH" ~ 39,
  qual1$state == "OK" ~ 40,
  qual1$state == "ON" ~ 41,
  qual1$state == "OR" ~ 42,
  qual1$state == "PA" ~ 43,
  qual1$state == "PR" ~ 44,
  qual1$state == "QC" ~ 45,
  qual1$state == "RI" ~ 46,
  qual1$state == "SC" ~ 47,
  qual1$state == "SD" ~ 48,
  qual1$state == "TN" ~ 49,
  qual1$state == "TX" ~ 50,
  qual1$state == "UT" ~ 51,
  qual1$state == "VA" ~ 52,
  qual1$state == "WA" ~ 53,
  qual1$state == "WI" ~ 54,
  qual1$state == "WV" ~ 55,
  qual1$state == "WY" ~ 56,
  qual1$state == "ZZ" ~ 57)

#Convert acctstd to numbers
#unique(qual1$acctstd)
qual1$acctstd <- case_when(
  qual1$acctstd == "DI" ~ 1,
  qual1$acctstd == "DS" ~ 2,
  qual1$acctstd == "DU" ~ 3,
  qual1$acctstd == "ZZ" ~ 4
)


qual1 <- qual1 %>% mutate(liqratio= act/lct)
qual1 <- qual1 %>% mutate(dtaratio= dltt/at)

#colnames(qual1)[colSums(is.na(qual1)) > 0]
qual1$liqratio <- ifelse(qual1$liqratio == "NaN"|qual1$liqratio == "Inf",0,qual1$liqratio)
qual1$dtaratio <- ifelse(qual1$dtaratio == "NaN"|qual1$dtaratio == "Inf" ,0,qual1$dtaratio)

#Make a dataframe that contains only the target row for this class.
#My gvkey is 25119 for Mohawk Inc
MHK <- qual1 %>% filter(gvkey == 25119)
#later you're going to scale this df

#colnames(qual1)[colSums(is.na(qual1)) > 0]

#========= Look for outliers =========#

qual_normchecks <- lm(SettleYN ~., data = qual1)
summary(qual_normchecks)
sm <- summary(qual_normchecks)
MSE <-  mean(sm$residuals^2)
MSE
par(mfrow=c(2, 2))
plot(qual_normchecks, main = "Initial Cleansed Data Set")
par(mfrow=c(1, 2))

#========= DEAL WITH OUTLIERS =========#
#Break the data into two sets. One with settlements and one without
SttlRows <- qual1 %>% filter(SettleYN > 0)
NoSttlRows <- qual1 %>% filter(SettleYN == 0)
#create a df to hold the categorical fields
NoSttlRows_cats <- NoSttlRows[,c(1:13)]

#Remove outliers from data set with no Settlement rows
outliers <- function(x) {
  Q1 <- quantile(x, probs=.1)
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
NoSttlRows <- remove_outliers(NoSttlRows[,c(2,14:64)])
#merge the categorical fields back with the continuous fields that now have outliers removed
qual1 <- merge(NoSttlRows_cats,NoSttlRows,by="gvkey")
#Union back with the rows that have a settlement
qual1 <- rbind(qual1, SttlRows)
rm(NoSttlRows, SttlRows, NoSttlRows_cats)

#colnames(qual1)[colSums(is.na(qual1)) > 0]
#NACounts <- as.data.frame(colSums(is.na(quant1)))

qual_normchecks <- lm(SettleYN ~., data = qual1)
summary(qual_normchecks)
sm <- summary(qual_normchecks)
MSE <-  mean(sm$residuals^2)
MSE
par(mfrow=c(2, 2))
plot(qual_normchecks, main = "Outliers Removed Data Set")

#Make Training and Test sets
QLtraining_ind <- createDataPartition(qual1$SettleYN, p = 0.7, list = FALSE, times = 1) 
QLtrain <- qual1[QLtraining_ind, ]
QLtest <- qual1[-QLtraining_ind, ]
QLtestMHK <- rbind(MHK,QLtest)


QLtrainscl <- scale(QLtrain)
scaleList <- list(scale = attr(QLtrainscl, "scaled:scale"),
                               center = attr(QLtrainscl, "scaled:center"))
QLtrainscl <- as.data.frame(scale(QLtrain))

QLtestscl <- as.data.frame(scale(QLtest,
                               center = apply(QLtrain, 2, mean),
                               scale = apply(QLtrain, 2, sd)
))

QLtestMHKscl <- as.data.frame(scale(QLtestMHK,
                                 center = apply(QLtrain, 2, mean),
                                 scale = apply(QLtrain, 2, sd)
))

QLMHKscl <- as.data.frame(scale(MHK,
                              center = apply(QLtrain, 2, mean),
                              scale = apply(QLtrain, 2, sd)))

trainmean <- mean(as.numeric(QLtrainscl$SettleYN))
trainsd <- sd(as.numeric(QLtrainscl$SettleYN))

#We want to scale the data set but retain the response variable as a 0/1 factor.
#Here we check what the response variable has been scaled to and convert it back to an 0/1 factor.
QLtrainscl$SettleYN <- as.factor(ifelse(QLtrainscl$SettleYN <= 0, 0,1))
QLtestscl$SettleYN <- as.factor(ifelse(QLtestscl$SettleYN <= 0, 0,1))
QLtestMHKscl$SettleYN <- as.factor(ifelse(QLtestMHKscl$SettleYN <= 0, 0,1))
QLMHKscl$SettleYN <- as.factor(ifelse(QLMHKscl$SettleYN <= 0, 0,1))

#GLM FIT
glm.fit <- glm(SettleYN ~ 
                 avg30_cshoc	 +
                 spcsrc +
                 liqratio
               ,data = QLtrainscl
               ,family = binomial)

summary(glm.fit)
probs_glm <- predict(glm.fit, QLtestscl, type = "response")
pred.glm <- rep("0", length(probs_glm))
pred.glm[probs_glm > 0.50] <- "1"
ConfMat.glm <- table(pred.QLTr, QLtestscl$SettleYN)
ConfMat.glm 

CM.GLM <- (32+4)/(32+4+11)
CM.GLM

MSE.GLM <- sum(glm.fit$residuals^2)/glm.fit$df.residual
MSE.GLM

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#Validate the predict function applies the logit function for glm fits
coef(glm.fit)

glm.respscl <-  -0.6839253 + 
  QLMHKscl$avg30_cshoc*7.6743579 + 
  QLMHKscl$spcsrc*1.1136490 + 
  QLMHKscl$liqratio*0.7960210

glm.respscl 

glm.logit.resp.scl <- logit2prob(glm.respscl )
glm.logit.resp.scl

unspred.glmMHK <- (glm.logit.resp.scl*trainsd)+trainmean
likeEst.glm <- unspred.glmMHK

par(mfrow=c(1, 1))
rocr_pred.glm <- prediction(probs_glm , QLtestscl$SettleYN) 
rocr_roc.glm <- performance(rocr_pred.glm, measure = "tpr", x.measure = "fpr") 
plot(rocr_roc.glm, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)

rocr_auc.glm <- performance(rocr_pred.glm, measure = "auc") 
auc.glm <- rocr_auc.glm @y.values[[1]] 
auc.glm

#I don't know exactly what is happening here, but when I run the model
#against the scaled MHK-only data set I get the same predicted value
#as the unscaled MHK prediction generated by the formula from the test set.
#This seems like a good thing, but I don't know how this works.
spMHK <- predict(glm.fit, QLMHKscl, type = "response")
uspMHK <- predict(glm.fit, MHK, type = "response")
logit2prob(uspMHK)
logit2prob(linr.respscl)

##=========== LDA ===========

lda.fit <- lda(SettleYN ~ 
                 avg30_cshoc	 +
                 spcsrc +
                 liqratio 
               , data = QLtrainscl)
lda.fit
summary(lda.fit)
pred.LDA <- data.frame(predict(lda.fit, QLtestscl))
table(pred.LDA$class, QLtestscl$SettleYN)
print(lda.fit)
plot(lda.fit)
#Compute correct classification rate
CM.LDA = (32+3)/(32+3+12)
CM.LDA

#For LDA we want to look at the posterior probabilities to see what class 
#a given data point has been assigned to. 
#This formula calculates the LD1 value for MHK
unspred.ldaMHK <-  QLMHKscl$avg30_cshoc*0.7465578 + 
  QLMHKscl$spcsrc*0.7036389 + 
  QLMHKscl$liqratio*0.6046063
unspred.ldaMHK
#It comes to 0.9263292

#Now I want to confirm the above LD1 value and also find its posterior probabilities
#AND the class its been assigned to.

#First, run your LDA model against a test set that has the MHK row as the first row
#The MHK row was intentionally put at the top of the data set just to make it easier to find
#We did this around line 189 in the code: QLtestMHK <- rbind(MHK,QLtest)
pred.LDAMHK <- data.frame(predict(lda.fit, QLtestMHKscl))
head(pred.LDAMHK)

pred.LDAMHKs <- data.frame(predict(lda.fit, QLMHKscl))
pred.LDAMHKs <- as.data.frame(pred.LDAMHKs)

likeEst.lda <- pred.LDAMHKs$posterior.1

#The head function returns the first row, which we see has an LD1 that matches lda.respscl
#0.9263292

# head(pred.LDAMHK)
# class posterior.0 posterior.1        LD1
# 1      0   0.5808535  0.41914650  0.9263293
# 4      0   0.9727331  0.02726687 -1.4032722
# 8      0   0.9727331  0.02726687 -1.4032722

#So here we can see the posterior probabilities of the class assignment.
#We see that our MHK row with LD1 = .09263 was assigned to class 0 with a 58% probability.
#From what I can gather this means the model is about 58% sure MHK belongs to class 0,
#or it is 58% sure MHK will *not* be sued.
#This means the model is 41.9% sure MHK *will* be sued.
#So the answer to our likelihood estimation is 41.9% chance of being sued.
#I don't think there is any need to unscale anything here.

par(mfrow=c(1, 1))
rocr_pred.lda <- prediction(pred.LDA$posterior.1, QLtestscl$SettleYN) 
rocr_roc.lda <- performance(rocr_pred.lda, measure = "tpr", x.measure = "fpr") 
plot(rocr_roc.lda, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)

rocr_auc.lda <- performance(rocr_pred.lda, measure = "auc") 
auc.lda <- rocr_auc.lda@y.values[[1]] 
auc.lda

##=========== QDA ===========
#Method differs from LDA but produces consistent results.
qda.fit <- train(SettleYN ~ avg30_cshoc + spcsrc + liqratio
                 , method = "qda", data = QLtrainscl)
pred.QDA <- predict(qda.fit, QLtestscl)
CM.QDA <-confusionMatrix(QLtestscl$SettleYN, predict(qda.fit, QLtestscl))
CM.QDA.acc <- as.data.frame(as.matrix(CM.QDA$overall))
CM.QDA.acc <- head(CM.QDA.acc,1)
CM.QDA <- CM.QDA.acc$V1

#We don't get posterior probabilities here, just an overall classification rate
#produced by the CM. So, I just ran the MHK data throuhg the model and the model
#classified it as a 0 or not sued. Since the CM gives us a correct rate of about
#76.6% I guess we just have to surmise that there is a (1-.7659) = .2341 or 23.4%
#chance that MHK *will* be sued.
#Really not sure if that's the right way to think about this.
pred.QDAMHK <- predict(qda.fit, QLMHKscl)
pred.QDAMHK <- as.data.frame(pred.QDAMHK)
likeEst.qda <- (1-CM.QDA)

##============ KNN =====================
QLtrainscl_knn <- QLtrainscl
QLtestscl_knn <- QLtestscl
QLtestMHKscl_knn <- QLtestMHKscl

QLtrainscl_knn$SettleYN <- as.integer(QLtrainscl_knn$SettleYN)
QLtestscl_knn$SettleYN <- as.integer(QLtestscl_knn$SettleYN)

trainX <- as.matrix(QLtrainscl_knn)
testX <- as.matrix(QLtestscl_knn)

trainscl_SettleYN <- QLtrainscl$SettleYN
testscl_SettleYN <- QLtestscl_knn$SettleYN

knn.fit <- knn(trainX, testX, cl=trainscl_SettleYN, k = 9)
predict(knn.fit, QLtestscl_knn)
table(knn.fit, testscl_SettleYN)
mean(knn.fit == testscl_SettleYN)
#Compute Correct classification rate
CM.KNN = (32+6)/(32+6+9)
CM.KNN
likeEst.knn <- CM.KNN

set.seed(1422)
model.knn <- train(SettleYN ~ .,
                   data = QLtrainscl,
                   method = "knn")
print(model.knn)

pred_knn <- predict(model.knn, QLtestscl)
pred.acc.knn <- sum(pred_knn == QLtestscl$SettleYN)/length(QLtestscl$SettleYN)

#This situation is similar to QDA where we don't get an individual % estimate, we just get
#accuracy measures from the CM. Here, the knn fit has an accuracy rate of 80.9%.
#Since the model sorted MHK into the 0/not sued category, we could say that 
#MHK has a (1-.8085) = .1915 chance of being sued, or 19.15% chance of being sued.
pred.knnMHK <- predict(model.knn, QLMHKscl)
likeEst.knn <- (1-CM.KNN)


##============Model Confusion Matrices Comparison ===========================
Model <- c("Logistic Regression"
           ,"LDA"
           ,"QDA"
           ,"KNN")
CM <-c(CM.GLM
        ,CM.LDA
        ,CM.QDA
        ,CM.KNN)
CMs <- cbind(Model,CM)
CMs

par(mfrow=c(1, 1))
par(mar=c(8, 4, 4, 2) + 0.1)
bp<-barplot(c(CM.GLM
              ,CM.LDA
              ,CM.QDA
              ,CM.KNN),
            col = "lightgreen", names.arg = c("Logistic Regression"
                                             ,"LDA"
                                             ,"QDA"
                                             ,"KNN"),
            main = "Confusion Matrices by Models",las=1)
text(bp, 0, format(round(c(CM.GLM
                           ,CM.LDA
                           ,CM.QDA
                           ,CM.KNN),4),big.mark=",", trim=TRUE),cex=1,pos=3)

##=================Likelihood Estimates Comparison ===================
Model <- c("Logistic Regression"
           ,"LDA"
           ,"QDA"
           ,"KNN")
LE <-round(c(likeEst.glm
       ,likeEst.lda
       ,likeEst.qda
       ,likeEst.knn),4)
LEs <- cbind(Model,LE)
LEs

par(mfrow=c(1, 1))
par(mar=c(8, 4, 4, 2) + 0.1)
bp<-barplot(c(likeEst.glm
              ,likeEst.lda
              ,likeEst.qda
              ,likeEst.knn),
            col = "lightblue", names.arg = c("Logistic Regression"
                                             ,"LDA"
                                             ,"QDA"
                                             ,"KNN"),
            main = "Likelihood Estimate by Model",las=1)
text(bp, 0, format(round(c(likeEst.glm
                           ,likeEst.lda
                           ,likeEst.qda
                           ,likeEst.knn),4),big.mark=",", trim=TRUE),cex=1,pos=3)

auc.glm
auc.lda

plot(rocr_roc.glm, col = "red")
plot(rocr_roc.lda, add = TRUE, col = "blue")
plot(p3, add = TRUE, col = "green")

par(mfrow=c(1, 1))
reset.par()
DSQLratio <- as.data.frame(table(DS_QL$SettleYN))
DSQLhist <- barplot(table(DS_QL$SettleYN)
                 , col="maroon"
                 , ylim=c(0,1050)
                 ,main = "Company Settlement Ratio in Initial Data"
                 ,names.arg = c("No Settlement"
                                ,"Settlement"))
text(DSQLhist,0,c(DSQLratio$Freq),cex=1,pos=3)

qual1ratio<- as.data.frame(table(qual1$SettleYN))
qual1hist <- barplot(table(qual1$SettleYN)
                     , col="darkmagenta"
                     , ylim=c(0,120)
                     ,main = "Company Settlement Ratio in Final Data"
                     ,names.arg = c("No Settlement"
                                    ,"Settlement"))
text(DSQLhist,0,c(qual1ratio$Freq),cex=1,pos=3)

