require(plyr)
library(caret)
require(kernlab)
library(e1071)
require(MASS)
require(mclust)
<<<<<<< HEAD
AGN_data<-read.table("../data/sample_WHAN_BPT_rafa.dat",header=F,sep="")

# Format data for WHAN test
WHAN<-AGN_data[,c(9,10,11)]
colnames(WHAN)<-c("x_WHAN","y_WHAN","class_WHAN")
WHAN$class_WHAN<-as.factor(WHAN$class_WHAN)
=======
AGN_data<-read.table("../data/outputdata_all-1.txt",header=TRUE,sep="")

# Format data for WHAN test
WHAN<-AGN_data[,c("log10..NII..Ha.","log10.EW.Ha..","WHAN_Class")]
WHAN$WHAN_Class<-as.factor(WHAN$WHAN_Class)
>>>>>>> origin/master
#write.matrix(WHAN,"../data/WHAN.txt")




data<-WHAN[,-3]
mod4 = Mclust(data,G=5)
summary(mod4)
plot(mod4, what = "classification")
plot(mod4, what = "boundaries", ngrid = 200)



ggplot(WHAN,aes(x=log10..NII..Ha.,y=log10.EW.Ha..,colour=WHAN_Class))+
  geom_point()+theme_stata()


library(fpc)
# eps is radius of neighborhood, MinPts is no of neighbors
# within eps
 cluster <- dbscan(data, eps=0.6, MinPts=4)
 plot(cluster, data)

# Notice points in cluster 0 are unassigned outliers
 table(cluster$cluster, WHAN$WHAN_Class)
 
 
 confusionMatrix(WHAN$WHAN_Class,mod4$classification-1)
 
 library("cluster")
 l <- mod4$classification
 d <- dist(WHAN[-3])
 
 si<-silhouette(mod4$classification,d)
 
 
 si2<-silhouette(as.numeric(WHAN$WHAN_Class),d)
 