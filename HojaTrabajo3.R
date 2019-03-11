library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n�mero de clusters �ptimo
library(factoextra) #Para hacer gr�ficos bonitos de clustering
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

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

#Herramienta para determinar el mejor numero de clusters
wss <- (nrow(perros5[,3:16])-1)*sum(apply(perros5[,3:16],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(perros5[,3:16], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")


#Paquete para saber el mejor n�mero de clusters
nb <- NbClust(perros5[,3:16], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")


#cluster 1
library(mclust)
fit <- Mclust(perros5)
plot(fit)
#cluster k-means
library(fpc)
datos<-perros5
perrosCompleto<-perros5[complete.cases(perros5),]
km<-kmeans(perros5[,3:16],4)
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
#hierarchical clustering
#Clustering jerárquico
hc<-hclust(dist(perros5[,3:16])) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


g1HC<-perros5[perros5$gruposHC==1,]
g2HC<-perros5[perros5$gruposHC==2,]
g3HC<-perros5[perros5$gruposHC==3,]


#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(perros5[,3:16]))
mean(silch[,3]) #0.75, no es la mejor partición pero no está mal

##para recoleccion de basura
gc()

#Arboles de decision

# variable respuesta la clase de la velocidad de adopcion
porciento <- 70/100

#install.packages("rpart")
#install.packages("rpart.plot")


library(rpart)
library(rpart.plot)

prueba <- perros5[,3:15]
prueba[14] <- perros5[24]

set.seed(123)
tree <- rpart(AdoptionSpeed ~ ., data = prueba, method = "class")
rpart.plot(tree)
sample(1:10,3)
muestra <- sample(1:nrow(prueba),0.7*nrow(prueba))
length(muestra)
train <- prueba[muestra,] #70% entrenamiento
test<- prueba[-muestra,] #30% prueba
muestra
tree <- rpart(AdoptionSpeed ~ ., data = train, method = "class")
rpart.plot(tree)
tree
clasificacion <- predict(tree,newdata = test[,1:13])

#basandonos en lo de lynette 
datos <- prueba

porciento <- 70/100

set.seed(124)

trainRowsNumber<-sample(1:nrow(datos),porciento*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]
dt_model<-rpart(AdoptionSpeed~.,train,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)

head(test)
prediccion <- predict(dt_model, newdata = test[1:13])

#Apply: Para cada fila, determina el nombre de la columna del valor máximo entre los tres valores de una fila
columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
test$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción
test$prediccion <- as.numeric(test$prediccion)
cfm<-confusionMatrix(  test$prediccion,test$AdoptionSpeed)
cfm

#prueba para solucionar lo de los niveles de factor. Como ignora los 0,1,3 esto lo soluciona
u <- union(test$prediccion, test$AdoptionSpeed)
t <- table(factor(test$prediccion, u), factor(test$AdoptionSpeed, u))
confusionMatrix(t)

#prueba

#con caret
ct<-trainControl(method = "cv",train[,1:13],number=3, verboseIter=T)
modelorf<-train(AdoptionSpeed~.,data=train,method="rf",trControl = ct)
prediccionRF<-predict(modelorf,newdata = test[,1:13],na.action = na.omit)
testCompleto<-test
testCompleto$predRFCaret<-prediccionRF
cfmCaret <- confusionMatrix(testCompleto$predRFCaret,testCompleto$AdoptionSpeed)

#prueba para solucionar lo de los niveles de factor. Como ignora los 0,1,3 esto lo soluciona
u <- union(testCompleto$predRFCaret, testCompleto$AdoptionSpeed)
t <- table(factor(testCompleto$predRFCaret, u), factor(testCompleto$AdoptionSpeed, u))
confusionMatrix(t)

#prueba

#con random forest
modeloRF1<-randomForest(AdoptionSpeed~.,data=train)
prediccionRF1<-predict(modeloRF1,newdata = test)
testCompleto<-test
testCompleto$predRF<-prediccionRF1
cfmRandomForest <- confusionMatrix(testCompleto$predRF, testCompleto$AdoptionSpeed)

#prueba para solucionar lo de los niveles de factor. Como ignora los 0,1,3 esto lo soluciona
u <- union(testCompleto$predRF, testCompleto$AdoptionSpeed)
t <- table(factor(testCompleto$predRF, u), factor(testCompleto$AdoptionSpeed, u))
confusionMatrix(t)

#prueba
