##load modules and data
library("ggplot2")


##laptop
CrossingGC <- read.csv("C:/Users/cul07b/Dropbox/PhD/Crossing/Crossing F2 GC - Combined.csv", header=TRUE)
CrossingGC <- read.csv("C:/Users/cul07b/Dropbox/PhD/Crossing/Crossing F1 GC - Combined.csv", header=TRUE)


##desktop
CrossingGC <- read.csv("E:/Data/Dropbox/PhD/Crossing/Crossing F2 GC - Combined.csv", header=TRUE)
CrossingGC <- read.csv("E:/Data/Dropbox/PhD/Crossing/Crossing F1 GC - Combined.csv", header=TRUE)


##create a new column that calculates whether 18:1 < 18:2 - F2
CrossingGC$lowOleic <- ifelse((CrossingGC$C18.1 < CrossingGC$C18.2),paste(c("TRUE")),paste(c("FALSE")))

##create a new column that calculates whether 18:1 < 18:2 - F1
CrossingGC$lowOleic <- ifelse((CrossingGC$C18.1d6 < CrossingGC$C18.2),paste(c("TRUE")),paste(c("FALSE")))


##Dual histogram for 18:1 and 18:2 content: 
par(mfrow=c(1, 2))
colnames <- dimnames(CrossingGC)[[2]]

for (i in c("C18.1","C18.2")) {
  hist(CrossingGC[,i], breaks=100, main=colnames[i], col="orange", border="white",freq=TRUE,xlab=c("Oil Content"),xlim=c(0,100))
}


#Label the plants based on either Cross, or as S317 or Chinese parent - F2
CrossingGC$PlantType <- ifelse(substring(CrossingGC$Sample,1,1)=="X",paste(c("Cross")),(ifelse((substring(CrossingGC$Sample,1,1))=="C",paste(c("Chinese")),paste(c("S317")))))


CrossingGC$PlantType <- ifelse((as.character(CrossingGC$Male) == as.character(CrossingGC$Female)),paste(CrossingGC$Male),paste(c("Cross")))


##cluster analysis
fit_4 <- kmeans(CrossingGC$C18.1,4)
aggregate(CrossingGC$C18.1,by=list(fit_4$cluster),FUN=mean)
CrossingGC$kmeans_4 <- fit$cluster

fit_3 <- kmeans(CrossingGC$C18.1,3)
aggregate(CrossingGC$C18.1,by=list(fit_3$cluster),FUN=mean)
CrossingGC$kmeans_3 <- fit_3$cluster


fit_3 <- kmeans(CrossingGC$C18.1d6,3)
aggregate(CrossingGC$C18.1d6,by=list(fit_3$cluster),FUN=mean)
CrossingGC$kmeans_3 <- fit_3$cluster



##ggplot dot point histogram - not quite right...
dotplot <-ggplot(data = CrossingGC)
dotplot <-dotplot + aes(x=C18.1,fill=ifelse(substring(CrossingGC$Sample,1,1)=="X","orange","blue"))
dotplot + geom_dotplot(dotsize = 1.5,binwidth = 0.70, method='histodot')


##scatter plot of C18:1 to C18:2
dotplot2 <-ggplot(CrossingGC,aes(CrossingGC$C18.1,CrossingGC$C18.2,color=CrossingGC$PlantType,xmin=0,xmax=100,ymin=0,ymax=100))
dotplot2 + geom_point() + scale_y_continuous("Quantity of Linoleic Acid (18:2) (%)",expand = c(0,0), breaks = seq(0,100,by=20)) + scale_x_continuous("Quantity of Oleic Acid (18:1) (%)",expand = c(0,0),breaks = seq(0,100,by=20)) + labs(color="Cultivar")


##scatter plot of C18:1 to C18:2 - F1
dotplot2 <-ggplot(CrossingGC,aes(CrossingGC$C18.1d6,CrossingGC$C18.2,color=CrossingGC$PlantType,xmin=0,xmax=100,ymin=0,ymax=100))
dotplot2 + geom_point() + scale_y_continuous("Quantity of Linoleic Acid (18:2) (%)",expand = c(0,0), breaks = seq(0,100,by=20)) + scale_x_continuous("Quantity of Oleic Acid (18:1) (%)",expand = c(0,0),breaks = seq(0,100,by=20)) + labs(color="Cultivar")



##scatter plot of C18:1 to C18:2 with clustering 4
dotplot2 <-ggplot(CrossingGC,aes(CrossingGC$C18.1,CrossingGC$C18.2,color=CrossingGC$PlantType,shape=as.character(CrossingGC$kmeans_4),xmin=0,xmax=100,ymin=0,ymax=100))
dotplot2 + geom_point() + scale_y_continuous("Quantity of Linoleic Acid (18:2) (%)",expand = c(0,0), breaks = seq(0,100,by=20)) + scale_x_continuous("Quantity of Oleic Acid (18:1) (%)",expand = c(0,0),breaks = seq(0,100,by=20)) + labs(color="Cultivar",shape="Group")


##scatter plot of C18:1 to C18:2 with clustering 3 and clustering named by cluster elements n
dotplot2 <-ggplot(CrossingGC,aes(CrossingGC$C18.1,CrossingGC$C18.2,shape=as.character(CrossingGC$kmeans_3),color=CrossingGC$PlantType,xmin=0,xmax=100,ymin=0,ymax=100))
dotplot2 + geom_point() + scale_shape_discrete(labels=c(paste("A, n =",sum(CrossingGC$PlantType=="Cross"&CrossingGC$kmeans_3=="1")),paste("B, n =",sum(CrossingGC$PlantType=="Cross"&CrossingGC$kmeans_3=="2")),paste("C, n =",sum(CrossingGC$PlantType=="Cross"&CrossingGC$kmeans_3=="3")))) + scale_y_continuous("Quantity of Linoleic Acid (18:2) (%)",expand = c(0,0), breaks = seq(0,100,by=20)) + scale_x_continuous("Quantity of Oleic Acid (18:1) (%)",expand = c(0,0),breaks = seq(0,100,by=20)) + labs(color="Cultivar",shape="Clusters\n(n=408)")


##scatter plot of C18:1 to C18:2 with clustering 3 and clustering named by cluster elements n - F1
dotplot2 <-ggplot(CrossingGC,aes(CrossingGC$C18.1d6,CrossingGC$C18.2,shape=as.character(CrossingGC$kmeans_3),color=CrossingGC$PlantType,xmin=0,xmax=100,ymin=0,ymax=100))
dotplot2 + geom_point() + scale_shape_discrete(labels=c(paste("A, n =",sum(CrossingGC$PlantType=="Cross"&CrossingGC$kmeans_3=="1")),paste("B, n =",sum(CrossingGC$PlantType=="Cross"&CrossingGC$kmeans_3=="2")),paste("C, n =",sum(CrossingGC$PlantType=="Cross"&CrossingGC$kmeans_3=="3")))) + scale_y_continuous("Quantity of Linoleic Acid (18:2) (%)",expand = c(0,0), breaks = seq(0,100,by=20)) + scale_x_continuous("Quantity of Oleic Acid (18:1) (%)",expand = c(0,0),breaks = seq(0,100,by=20)) + labs(color="Cultivar",shape="Clusters\n(n=84)")



##scatter plot of C18:1 to C16:0
dotplot3 <-ggplot(CrossingGC,aes(CrossingGC$C18.1,CrossingGC$C16.0,color=CrossingGC$PlantType,xmin=0,xmax=100,ymin=2,ymax=7))
dotplot3 + geom_point() + scale_y_continuous("Quantity of 16:0) (%)",expand = c(0,0), breaks = seq(2,7,by=1)) + scale_x_continuous("Quantity of Oleic Acid (18:1) (%)",expand = c(0,0),breaks = seq(0,100,by=20)) + labs(color="Cultivar")


