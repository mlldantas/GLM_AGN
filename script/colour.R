require(plyr)
library(caret)
require(kernlab)
library(e1071)
require(MASS)
require(mclust)
data<-read.table("../data/outputdata_all.txt",header=TRUE,sep="")

# Format data for WHAN test
color_data<-data[,c("mag_r_abs","mag_ab_g","mag_ab_r")]
#WHAN$WHAN_Class<-as.factor(WHAN$WHAN_Class)
#write.matrix(WHAN,"../data/WHAN.txt")
color_data$g.r<-color_data$mag_ab_g-color_data$mag_ab_r



#data<-WHAN[,-3]
mod4 = Mclust(color_data[,c("mag_r_abs","g.r")])
summary(mod4)
plot(mod4, what = "classification")
plot(mod4, what = "boundaries", ngrid = 200)

densWaiting <- densityMclust(color_data[,c("mag_r_abs","g.r")])


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
