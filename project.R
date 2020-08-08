install.packages("animation")
library(animation)
library(vegan)
library(permute)
library(lattice)
library(cluster)
library(latticeExtra)

data<-read.csv(choose.files())
View(data)
names(data)
summary(data)
kmeans.ani(data,2)

#########Data Scaling

data_scaled<-scale(data)
View(data_scaled)
names(data_scaled)
head(data_scaled)
summary(data_scaled)

kmdata<-kmeans(data_scaled,centers = 2)
class(kmdata)

dist.res=dist(data_scaled,method = "euclidean")
hc=hclust(dist.res,method = "complete")

######Visualize h_clust

plot(hc,labels = FALSE,hang = -1)
rect.hclust(hc,k=3,border = 2:3)

fit<-cascadeKM(scale(data,center = TRUE,scale = TRUE),1,10,iter=1000)
plot(fit,sortg = TRUE,grpmts.plot = TRUE)

calinski.best<-as.numeric(which.max(fit$results[2,]))
cat("Calinkshi criterion optimal number of clusters:",calinski.best,"\n")


#######Elbow Chart

mydata<-data
wss<-(nrow(mydata)-1)*sum(apply(mydata, 2, var))

for (i in 2:15)wss[i]<-sum(kmeans(mydata,centers = i)$withinss) 
plot(1:15,wss,type = "b",xlab ="Number of clusters",ylab="Within group of sum of squres ",col="mediumseagreen",pch=12)  

k1<-kmeans(data,2)
k1
k1$centers
k1$size

data$cluster<-k1$cluster

diss=daisy(data_scaled)
sp=silhouette(data$cluster,diss)
plot(sp)

aggregate(.~cluster,data = data,mean)

kmeans.ani(data,2)

marginal.plot(data)
names(data)[13]<-"class"
myfile3<-data

library(randomForest)
library(caret)
library(pROC)
library(e1071)

sp_data=myfile3
View(sp_data)
set.seed(1234)

index<-createDataPartition(sp_data$class,p=.70,list = FALSE  ,times = 1)
trainindex<-sp_data[index,]
testindex<-sp_data[-index,]

print(table(trainindex$class))
print(table(testindex$class))

model<-randomForest(as.factor(class)~.,data = trainindex,do.trace=T)
model

importance(model)
varImpPlot(model)


####PREDICTION

pred_tr<-predict(model,trainindex)
pred_test<-predict(model,testindex)
names(pred_test)
trainindex

confusionMatrix(as.factor(pred_tr),as.factor(trainindex$class))
confusionMatrix(as.factor(pred_test),as.factor(testindex$class))

auc_tr<-roc(as.numeric(trainindex$class),as.numeric(pred_tr),ci=TRUE)
auc_tr<-roc(as.numeric(testindex$class),as.numeric(pred_test),ci=TRUE)
