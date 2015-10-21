require(arm)
require(plyr)
library(caret)
library(pROC)
require(kernlab)
library(e1071)
require(rpart)
AGN_data<-read.table("../data/outputdata.txt",header=TRUE,sep="")
AGN_data$WHAN_Class<-as.factor(AGN_data$WHAN_Class)
AGN_data$WHAN_Class<-revalue(AGN_data$WHAN_Class,c("2"="AGN","3"="AGN","0"="No AGN","1"="No AGN","4"="No AGN"))



nonlinear.svm <- ksvm( WHAN_Class~log10.NII.Ha.+log10.EW.Ha.., data=AGN_data, type='spoc-svc', kernel='rbf',
                       kpar=list(sigma=1), C=100, scale=c() )
plot( nonlinear.svm, data=AGN_data )

fitted(nonlinear.svm)



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