require(plyr)
library(caret)
require(kernlab)
library(e1071)
require(MASS)
require(mclust)
AGN_data<-read.table("../data/outputdata_diagnostic.txt",header=TRUE,sep="")

# Format data for WHAN test
WHAN<-AGN_data[,c("log10..NII..Ha.","log10.EW.Ha..","WHAN_Class")]
WHAN$WHAN_Class<-as.factor(WHAN$WHAN_Class)
#write.matrix(WHAN,"../data/WHAN.txt")




data<-WHAN[,-3]
mod4 = Mclust(data)
summary(mod4)
plot(mod4, what = "classification")


ggplot(WHAN,aes(x=log10..NII..Ha.,y=log10.EW.Ha..,colour=WHAN_Class))+
  geom_point()