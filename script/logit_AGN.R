# Logit regression with AGNs 

require(arm)
require(plyr)
library(caret)
library(pROC)
AGN_data<-read.table("../data/outputdata.txt",header=TRUE,sep="")
AGN_data$WHAN_Class<-as.factor(AGN_data$WHAN_Class)
AGN_data$WHAN_Class<-revalue(AGN_data$WHAN_Class,c("2"="1","3"="1","1"="0","4"="0"))





fit<-bayesglm(WHAN_Class~log10.NII.Ha.+log10.EW.Ha..,family=binomial(link="logit"),scaled=TRUE,
              data = AGN_data)


ROCF<- data.frame(True=AGN_data$WHAN_Class,predicted=predict(fit,type = "response"))
F1 <-roc(ROCF$True,ROCF$predicted)
coords(F1,x="best")[1]

ROCF$class<-ROCF$predicted
ROCF$class[which(ROCF$class>=coords(F1,x="best")[1])]<-1
ROCF$class[which(ROCF$class<coords(F1,x="best")[1])]<-0

confusionMatrix(ROCF$True, ROCF$class)


