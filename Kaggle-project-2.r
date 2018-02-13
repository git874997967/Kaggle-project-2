library(kknn)
library(class)
library(sqldf)
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

