#Data Puromycin 
View(Puromycin )
str(Puromycin)
P.kelas<-c(rep("treated",12),rep("untreated",11))
P.data<-data.frame(Puromycin[,1:2],state=P.kelas)
# Training dan Testing
genap.n<-2*(1:12)-1
P.latihan<-P.data[genap.n,]
P.uji<-P.data[-genap.n,]
# Melakukan Klasifikasi
P.knn<-knn(P.latihan[,-3],P.uji[,-3],P.latihan[,3],k=5)(table(P.knn,P.uji[,3]))
P.knn1<-knn(P.latihan[,-3],P.uji[,-3],P.latihan[,3],k=7)(table(P.knn1,P.uji[,3]))
P.knn3<-knn(P.latihan[,-3],P.uji[,-3],P.latihan[,3],k=10)(table(P.knn3,P.uji[,3])
# Plot Klasifikasi
pairs(P.uji[,1:2],pch=as.character(P.uji[,3]),col=c(3,2)[(P.uji$state!=P.knn)+1])
# KNN-CV
Puromycin.cv<-knn.cv(Puromycin[,-3],Puromycin[,3],k=5)
table(Puromycin[,3],Puromycin.cv)
#Package
intal.packages("caret")
library(caret)
intal.packages("e1071")
library(e1071)
trControl <- trainControl(method  = "cv",number  = 5)
fit <- train(state ~ .,method     = "knn",tuneGrid   = expand.grid(k = 1:10),trControl  = trControl,metric     = "Accuracy",
data=Puromycin)
fit
