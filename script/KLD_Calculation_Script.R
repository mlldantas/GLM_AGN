library(EMCluster)
library(MASS)

#read the data in and eliminate the class variable for the clustering process
dataSet = read.table("E:/Academics/Thesis/WHAN.txt", header=TRUE, quote="\"")
dataSet$WHAN_Class = as.factor(dataSet$WHAN_Class)
dataForClustering = dataSet[,-3]

#Perform EM clustering through EMCluster algorithm package
set.seed(100)
emobj = simple.init(dataForClustering, nclass=6)
clusterResult = emcluster(dataForClustering, emobj, assign.class=TRUE)

# Plotting the clustering results
plotem(clusterResult, dataForClustering,  main="EMCluster Classification centers = 6", 
      xlab = "log10..NII..Ha.", ylab="log10.EW.Ha..")

#separate individual clusters from clustering result
cluster1 = dataForClustering[(clusterResult$class==1),]
cluster2 = dataForClustering[(clusterResult$class==2),]
cluster3 = dataForClustering[(clusterResult$class==3),]
cluster4 = dataForClustering[(clusterResult$class==4),]
cluster5 = dataForClustering[(clusterResult$class==5),]
cluster6 = dataForClustering[(clusterResult$class==6),]

#separate individual classes from original classification
class1 = dataSet[(dataSet$WHAN_Class == 0),-3]
class2 = dataSet[(dataSet$WHAN_Class == 1),-3]
class3 = dataSet[(dataSet$WHAN_Class == 2),-3]
class4 = dataSet[(dataSet$WHAN_Class == 3),-3]
class5 = dataSet[(dataSet$WHAN_Class == 4),-3]

#From here on in the script, we will pick up a cluster and a class 
#and then perform the comparison between them using our algorithm

#This next section of code can be automated to output the final 
#matrix of comparisons. However, I didn't have time to spend time automating it.

#We will add a class label column to both cluster and class to differentiate
#between them when performing LDA

#create a label vector for cluster and bind it as a column to the cluster matrix
clusterSize = dim(cluster4)[1]
label = rep('A', clusterSize)
cluster = cbind(cluster4, label)

#create a class vector for class and bind it as a column to the class matrix
classSize = dim(class3)[1]
label = rep('B', classSize)
class = cbind(class3, label)

#Perform LDA on data containing both cluster and class
data = rbind(cluster, class)
ldaResult = lda(label ~ ., data)
prediction =  predict(ldaResult)

#Getting projected matrices for cluster and class from predicted values
clusterProjected = prediction$x[1:clusterSize,]
classProjected = prediction$x[(clusterSize+1):(dim(data)[1]),]

#Getting densities of the cluster and class distributions
pdfCluster = density(clusterProjected)
pdfClass = density(classProjected)

#Extending the range so that both densities are withing minimum 
#and maximum of obtained density values
minRange = min(clusterProjected, classProjected)
maxRange = max(clusterProjected, classProjected)

pdfCluster = density(clusterProjected, from= minRange-1, to=maxRange+1)
pdfClass = density(classProjected, from= minRange-1, to=maxRange+1) 

#Get probability density from the densities
pdfCluster$y = pdfCluster$y/sum(pdfCluster$y)
pdfClass$y = pdfClass$y/sum(pdfClass$y)

#Plot the probability densities of cluster and class
plot(range(pdfCluster$x, pdfClass$x), range(pdfCluster$y, pdfClass$y), 
     type="n", xlab="X values for both distributions", ylab="Probability Density",
     main="Probability density plots")
lines(pdfClass, col="blue")
lines(pdfCluster, col="red")
legend("topleft", legend=c("Cluster", "Class"), col=c("red", "blue"), 
       lty=c(1,1))

#Create a vector for kl-distance calculation
klD.Cluster.Class = rep(Inf, length(pdfCluster$y))

#Calculate kl-distance for both distributions
for (i in 1:length(pdfCluster$y)) {
  if(pdfClass$y[i] != 0 && pdfCluster$y[i] != 0){
    klD.Cluster.Class[i] <- pdfCluster$y[i] * (log(pdfCluster$y[i]) - 
                                                 log(pdfClass$y[i]))
  }
}

#Obtain overall KL-distance as sum of values in kl-distance vector
klDistance = sum(klD.Cluster.Class[klD.Cluster.Class != Inf])

if (klDistance == 0)
  klDistance = Inf

#Output Kullback-Leibler distance between cluster and class
klDistance