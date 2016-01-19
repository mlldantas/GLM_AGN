require(arm)
require(plyr)
library(caret)
library(pROC)
require(kernlab)
library(e1071)
require(rpart)
AGN_data<-read.csv("../data/output_xray.csv",header=TRUE)
AGN_data<-AGN_data[AGN_data$F_nii>0 & AGN_data$F_Halpha > 0 & 
                     AGN_data$F_oiii > 0 & AGN_data$F_Hbeta > 0,]

AGN_data$x_bpt <- AGN_data$F_nii/AGN_data$F_Halpha
AGN_data$y_bpt <- AGN_data$F_oiii/AGN_data$F_Hbeta
AGN_data <- AGN_data[AGN_data$x_bpt>=-5 & AGN_data$x_bpt<= 5,]
AGN_data <- AGN_data[AGN_data$y_bpt>=-5 & AGN_data$y_bpt<= 5,]


nonlinear.svm <- svm(agn_type~y_bpt+x_bpt, data=AGN_data)
plot( nonlinear.svm,y_bpt~x_bpt, data=AGN_data,xlim=c(-1,1.5),ylim=c(-1.5,2.5))






fitted(nonlinear.svm)





AGN_data$WHAN_Class<-as.factor(AGN_data$WHAN_Class)
AGN_data$WHAN_Class<-revalue(AGN_data$WHAN_Class,c("2"="AGN","3"="AGN","0"="No AGN","1"="No AGN","4"="No AGN"))







linear.svm <- ksvm( WHAN_Class~log10.NII.Ha.+log10.EW.Ha.., data=AGN_data, cost = 100, gama = 1)
plot(linear.svm, data=AGN_data)


r <- rpart(WHAN_Class~log10.NII.Ha.+log10.EW.Ha.., data = AGN_data)

plot(r)
text(r) 
summary(r) 
plotcp(r)
printcp(r)
rsq.rpart(r)

plot(r) text(r) summary(r) plotcp(r) printcp(r) rsq.rpart(r)


library(evtree)

(ct = ctree(WHAN_Class~log10.NII.Ha.+log10.EW.Ha.., data = AGN_data))
plot(ct, main="Conditional Inference Tree")
ev.raw = evtree(WHAN_Class~log10.NII.Ha.+log10.EW.Ha.., data = AGN_data)
plot(ev.raw)

#Table of prediction errors
table(predict(ct), AGN_data$WHAN_Class)

# Estimated class probabilities

tr.pred = predict(ct, newdata=raw, type="prob")


require(mclust)
data<-data.frame(AGN_data$log10.NII.Ha.,AGN_data$log10.EW.Ha..)
mod4 = Mclust(data)
summary(mod4)
plot(mod4, what = "classification")
plot(mod4, what = "density", type = "image", 
     col = "dodgerblue3", grid = 100)