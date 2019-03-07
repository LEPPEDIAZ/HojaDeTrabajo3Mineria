perros <- read.csv("breed_labels.csv")
perros2 <- read.csv("color_labels.csv")
perros3 <- read.csv("state_labels.csv")
perros4 <- read.csv("sample_submission.csv")
#vamos a escoger esta data para trabajar
perros5 <- read.csv("train.csv")
summary(perros5)
#histogramas
hist(perros5$Type)
hist(perros5$Age)
hist(perros5$Breed1)
hist(perros5$Breed2)
hist(perros5$Gender)
hist(perros5$Color1)
hist(perros5$Color2)
hist(perros5$Color3)
hist(perros5$MaturitySize)
hist(perros5$FurLength)
hist(perros5$Vaccinated)
hist(perros5$Dewormed)
hist(perros5$Sterilized)
hist(perros5$Health)
hist(perros5$Quantity)
hist(perros5$Fee)
hist(perros5$State)
hist(perros5$VideoAmt)
hist(perros5$PetID)
hist(perros5$PhotoAmt)
hist(perros5$AdoptionSpeed)
#tablas de normalidad
qqnorm(perros5$Type)
qqline(perros5$Type, col = "steelblue", lwd = 2)
qqnorm(perros5$Age)
qqline(perros5$Age, col = "steelblue", lwd = 2)
qqnorm(perros5$Breed1)
qqline(perros5$Breed1, col = "steelblue", lwd = 2)
qqnorm(perros5$Breed2)
qqline(perros5$Breed2, col = "steelblue", lwd = 2)
qqnorm(perros5$Gender)
qqline(perros5$Gender, col = "steelblue", lwd = 2)
qqnorm(perros5$Color1)
qqline(perros5$Color1, col = "steelblue", lwd = 2)
qqnorm(perros5$Color2)
qqline(perros5$Color2, col = "steelblue", lwd = 2)
qqnorm(perros5$Color3)
qqline(perros5$Color3, col = "steelblue", lwd = 2)
qqnorm(perros5$MaturitySize)
qqline(perros5$MaturitySize, col = "steelblue", lwd = 2)
qqnorm(perros5$FurLength)
qqline(perros5$FurLength, col = "steelblue", lwd = 2)
qqnorm(perros5$Vaccinated)
qqline(perros5$Vaccinated, col = "steelblue", lwd = 2)
qqnorm(perros5$Dewormed)
qqline(perros5$Dewormed, col = "steelblue", lwd = 2)
qqnorm(perros5$Sterilized)
qqline(perros5$Sterilized, col = "steelblue", lwd = 2)
qqnorm(perros5$Health)
qqline(perros5$Health, col = "steelblue", lwd = 2) 
qqnorm(perros5$Quantity)
qqline(perros5$Quantity, col = "steelblue", lwd = 2)
qqnorm(perros5$Fee)
qqline(perros5$Fee, col = "steelblue", lwd = 2)
qqnorm(perros5$State)
qqline(perros5$State, col = "steelblue", lwd = 2)
qqnorm(perros5$VideoAmt)
qqline(perros5$VideoAmt, col = "steelblue", lwd = 2)
qqnorm(perros5$PetID)
qqline(perros5$PetID, col = "steelblue", lwd = 2)
qqnorm(perros5$PhotoAmt)
qqline(perros5$PhotoAmt, col = "steelblue", lwd = 2)
qqnorm(perros5$AdoptionSpeed)
qqline(perros5$AdoptionSpeed, col = "steelblue", lwd = 2)
#tablas de frecuencias
primertable <- with(perros5, table(Name))
margin.table(primertable, 1)
cuatrotable <- with(perros5, table(RescuerID))
segundotable <- with(perros5, table(PetID))
margin.table(segundotable, 1)
margin.table(cuatrotable, 1)
#clusters
#cluster 1
library(mclust)
fit <- Mclust(perros5)
plot(fit)
#cluster k-means
library(fpc)
datos<-perros5
perrosCompleto<-perros5[complete.cases(perros5),]
km<-kmeans(perros5[,3:16],3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Name))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Name))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$Name))*100
plotcluster(perros5[,3:16],km$cluster)
#gaussiano
mc<-Mclust(perros5[,3:16],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]
#fuzzy c-means
library(e1071)
fcm<-cmeans(perros5[,3:16],3)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)

