library(kknn)
library(class)
library(sqldf)
library(ranger)
library(randomForest)
library(InformationValue) 
library(dplyr)
library(party)
library(caret)
gost_train=read.csv(file.choose())
gost_test=read.csv(file.choose())
str(gost_train)
# attach(gost_train)
# missing=list(bone_length=nrow(data[is.na(bone_length),]))
# detach(gost_train)
# edit(gost_train)
levels(gost_train$type)

####color analysis
Ghost_color=sqldf("select count(*) as num,color,type from gost_train where type='Ghost' group by color ")
Ghoul_color=sqldf("select count(*) as num,color,type from gost_train where type='Ghoul' group by color ")
Goblin_color=sqldf("select count(*) as num,color,type from gost_train where type='Goblin' group by color ")
total_color=rbind(Ghost_color,Ghoul_color,Goblin_color)
color_matrix=cbind(Ghost_color,Ghoul_color,Goblin_color)
rownames(color_matrix)=color_matrix[,2]
color_matrix =color_matrix[,c(-2,-3,-5,-6,-8,-9)]
colnames(color_matrix)=levels(gost_train$type)
col=c(rgb(1,1,0,0.2),rgb(1,0,1,0.2),rgb(0,1,1,0.2))
barplot(t(color_matrix),beside=T,col=col,ylim =c(1,70))
legend(
  "topleft",
  levels(gost_train$type),
  fill=col
)
title("color and type",xlab=" gost type",ylab="gost num",ylim=100)
box()
gost_train$has_soul
###has soul analysis
####color analysis
Ghost_soul=sqldf("select count(*) as num,has_soul ,type from gost_train where type='Ghost' group by has_soul ")
Ghoul_soul=sqldf("select count(*) as num,has_soul,type from gost_train where type='Ghoul' group by has_soul ")
Goblin_soul=sqldf("select count(*) as num,has_soul,type from gost_train where type='Goblin' group by has_soul ")
total_soul=rbind(Ghost_soul,Ghoul_soul,Goblin_soul)
total_soul
plot(density(Ghost_soul$has_soul),ylim=c(0,3),xlim=c(-0.5,1.2),main="has_soul and type" ,xlab="has_soul value")
polygon(density(Ghost_soul$has_soul), col=rgb(1,1,0,0.2), border="blue", lwd=2) 
polygon(density(Ghoul_soul$has_soul), col=rgb(1,0,1,0.2), border="red", lwd=2) 
polygon(density(Goblin_soul$has_soul), col=rgb(0,1,1,0.2), border="red", lwd=2) 
legend(
  "topleft",
  levels(gost_train$type),
  fill=col
)
box()
####color analysis
gost_train$
Ghost_bone_length=sqldf("select bone_length, type from gost_train where type='Ghost' ")
Ghoul_bone_length=sqldf("select bone_length,type from gost_train where type='Ghoul' ")
Goblin_bone_length=sqldf("select bone_length,type from gost_train where type='Goblin'  ")
total_bone_length=rbind(Ghost_bone_length,Ghoul_bone_length,Goblin_bone_length)
total_soul
plot(density(Ghost_bone_length$bone_length),ylim=c(0,4),xlim=c(-0.25,1),main="bone_length and type" ,xlab="has_soul value")
polygon(density(Ghost_bone_length$bone_length), col=rgb(1,1,0,0.2), border="blue", lwd=2) 
polygon(density(Ghoul_bone_length$bone_length), col=rgb(1,0,1,0.2), border="red", lwd=2) 
polygon(density(Goblin_bone_length$bone_length), col=rgb(0,1,1,0.2), border="red", lwd=2) 
legend(
  "topleft",
  levels(gost_train$type),
  fill=col
)
box()
### rotting length
gost_train$rotting_flesh
Ghost_rotting_flesh=sqldf("select rotting_flesh, type from gost_train where type='Ghost' ")
Ghoul_rotting_flesh=sqldf("select rotting_flesh,type from gost_train where type='Ghoul' ")
Goblin_rotting_flesh=sqldf("select rotting_flesh,type from gost_train where type='Goblin'  ")
total_rotting_flesh=rbind(Ghost_rotting_flesh,Ghoul_rotting_flesh,Goblin_rotting_flesh)
total_soul
plot(density(Ghost_rotting_flesh$rotting_flesh),ylim=c(0,4),xlim=c(-0.1,1.1),main="rotting_flesh and type" ,xlab="has_soul value")
polygon(density(Ghost_rotting_flesh$rotting_flesh), col=rgb(1,1,0,0.2), border="blue", lwd=2) 
polygon(density(Ghoul_rotting_flesh$rotting_flesh), col=rgb(1,0,1,0.2), border="red", lwd=2) 
polygon(density(Goblin_rotting_flesh$rotting_flesh), col=rgb(0,1,1,0.2), border="red", lwd=2) 
legend(
  "topleft",
  levels(gost_train$type),
  fill=col
)
box()
###hair_length
gost_train$hair_length
Ghost_hair_length=sqldf("select hair_length, type from gost_train where type='Ghost' ")
Ghoul_hair_length=sqldf("select hair_length,type from gost_train where type='Ghoul' ")
Goblin_hair_length=sqldf("select hair_length,type from gost_train where type='Goblin'  ")
total_hair_length=rbind(Ghost_hair_length,Ghoul_hair_length,Goblin_hair_length)
total_soul
plot(density(Ghost_hair_length$hair_length),ylim=c(0,4),xlim=c(-0.1,1.2),main="hair_length and type" ,xlab="has_soul value")
polygon(density(Ghost_hair_length$hair_length), col=rgb(1,1,0,0.2), border="blue", lwd=2) 
polygon(density(Ghoul_hair_length$hair_length), col=rgb(1,0,1,0.2), border="red", lwd=2) 
polygon(density(Goblin_hair_length$hair_length), col=rgb(0,1,1,0.2), border="red", lwd=2) 
legend(
  "topleft",
  levels(gost_train$type),
  fill=col
)
box()
### data intergration

data=bind_rows(gost_train,gost_test)
train.row=1:nrow(gost_train)
test.row=1:nrow(gost_test)


## Use woe and Iv failed
#used in binary only!!!
#WOE(X=factor(data$bone_length[1:nrow(gost_train)]),Y=data$type[1:nrow(gost_train)])
#IV(X=factor(data$bone_length[1:nrow(gost_train)]),Y=data$type[1:nrow(gost_train)])

###  test 1
Ghost_feature2=Ghost_hair_length$hair_length*Ghost_bone_length$bone_length*Ghost_hair_length$hair_length*Ghost_soul$has_soul
Ghoul_feature2=Ghoul_hair_length$hair_length*Ghoul_bone_length$bone_length*Ghoul_hair_length$hair_length* Ghoul_soul$has_soul
Goblin_feature2=Goblin_hair_length$hair_length*Goblin_bone_length$bone_length*Goblin_hair_length$hair_length*Goblin_soul$has_soul
 
Ghost_feature=Ghost_hair_length$hair_length*Ghost_bone_length$bone_length*Ghost_hair_length$hair_length*Ghost_rotting_flesh$rotting_flesh*Ghost_soul$has_soul
Ghoul_feature=Ghoul_hair_length$hair_length*Ghoul_bone_length$bone_length*Ghoul_hair_length$hair_length*Ghoul_rotting_flesh$rotting_flesh*Ghoul_soul$has_soul
Goblin_feature=Goblin_hair_length$hair_length*Goblin_bone_length$bone_length*Goblin_hair_length$hair_length*Goblin_rotting_flesh$rotting_flesh*Goblin_soul$has_soul

plot(density(
  Ghost_feature2 
  ),ylim=c(0,100),xlim=c(-0.1,0.3),main="data features2  and type" ,xlab="has_soul value")
polygon(density(
  Ghost_feature2 
  ), col=rgb(1,1,0,0.2), border="blue", lwd=2) 
polygon(density(
  Ghoul_feature2 
  ), col=rgb(1,0,1,0.2), border="red", lwd=2) 
polygon(density(
  Goblin_feature2 
  ), col=rgb(0,1,1,0.2), border="red", lwd=2) 
legend(
  "topleft",
  levels(gost_train$type),
  fill=col
)
box()
###build the model with cforest
str(data)
set.seed(415)
#ok 33 33 34
#model_cforest <- cforest(type ~bone_length + rotting_flesh + hair_length + has_soul + color+bone_length*rotting_flesh*hair_length*has_soul, data = data[train.row, ], controls=cforest_unbiased(ntree=2000, mtry=3))
#ok 32 34 39 better!!!
#model_rpart <- rpart(type ~ color+bone_length + rotting_flesh + hair_length + has_soul , data = data[train.row, ],cp=0.0015,method='class')
# 35 34 42!!!!! Perfect?!
 model_ranger <- ranger(type ~ bone_length + rotting_flesh + hair_length + has_soul + color, data = data[train.row, ], num.trees=3000,num.threads=8)
 
 # model_kknn<- cforest(type ~ bone_length + rotting_flesh + hair_length + has_soul + color+bone_length*rotting_flesh*hair_length*has_soul, data = data[train.row, ])
# cv.summarize <- function(data.true, data.predict) {
#   print(paste('Recdata:', Recdata(data.true, data.predict)))
#   print(paste('Precision:', Precision(data.true, data.predict)))
#   print(paste('Accuracy:', Accuracy(data.predict, data.true)))
#   print(paste('AUC:', AUC(data.predict, data.true)))
# }
  
set.seed(415)
 cv.test.sample <- sample(1:nrow(gost_train), as.integer(0.3 * nrow(gost_train)), replace = TRUE)
 cv.test <- data[cv.test.sample,]
#cv.predictioncf <- predict(model_cforest, cv.test, OOB=TRUE, type = "response")
#cv.predictionrp <- predict(model_rpart, cv.test, OOB=TRUE, type = "class")
cv.predictionra <- predict(model_ranger,cv.test, interval = "prediction",  type = "response")

#cv.summarize(cv.test$type, cv.predictioncf)
table(cv.test$type,cv.predictionra$predictions)
sum(diag(prop.table( table(train$type,model_ranger$predictions) )))
 #print(model_rpart,digit=2)
 #plotcp(model_rpart,lty=1,col=2)
 
#finalcf=predict(model_cforest,data[(1+nrow(gost_train)):(nrow(data)),],OOB=T,type='response')
 finalranger=predict(model_ranger,data[(1+nrow(gost_train)):(nrow(data)),],OOB=T,type='response')
finalrpat=predict(model_rpart,data[(1+nrow(gost_train)):(nrow(data)),],OOB=T,type='class')
 ranger_result <- data.frame(id = gost_test$id, type = final$predictions)
 # rpart_result=data.frame(id=gost_test$id,type=finalrpat)
 # cf_result=data.frame(id=gost_test$id,type=finalcf)
  write.csv(cf_result, file = "ranger3.csv", row.names = FALSE)
 # table(gost_test,final$predictions)
 # finalcf
 