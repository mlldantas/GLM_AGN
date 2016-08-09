library(e1071);require(mclust);library(RColorBrewer)

AGN<-read.csv("https://raw.githubusercontent.com/mdastro/UV_Optical/master/Data/my_data_match.csv",header=T)
AGN<-AGN[AGN$NII>-900,]
AGN<-AGN[AGN$H_alpha>-900,]
AGN<-AGN[AGN$EW_H_alpha>-900,]

WHAN <- data.frame(log(AGN$NII/AGN$H_alpha,10),log(AGN$EW_H_alpha,10))

CLUST <- Mclust(WHAN)
plot(CLUST)

WHAN_new <- cbind(WHAN,CLUST$classification)
colnames(WHAN_new)<-c("x","y","class_type")




svm.model1 <- svm(class_type ~ y+x, data=WHAN_new, type='C-classification', kernel = 'linear')

plot(svm.model1,WHAN_new,col = brewer.pal(8, "Set3"))



svm.model2 <- svm(class_type ~ y+x, data=WHAN_new, type='C-classification', kernel = 'polynomial', degree=8, gamma=0.1, coef0=1)

plot(svm.model2,WHAN_new)

