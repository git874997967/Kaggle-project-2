train=read.csv(file.choose())
test=read.csv(file.choose())
train
all=bind_rows(train,test)
 
set.seed(123)

## 

# 两两组合生成新的变量     
all$bone_rot = all$bone_length * all$rotting_flesh
all$bone_hair = all$bone_length * all$hair_length
all$bone_soul = all$bone_length * all$has_soul
all$rot_hair = all$rotting_flesh * all$hair_length
all$rot_soul = all$rotting_flesh * all$has_soul
all$hair_soul = all$hair_length * all$has_soul

# 增加新的次方来生成新的变量
all$bone2 = all$bone_length ^ 2
all$rot2 = all$rotting_flesh ^ 2
all$hair2 = all$hair_length ^ 2
all$soul2 = all$has_soul ^ 2
str(all)

train=all[1:nrow(train),]
  
test=all[(1+nrow(train)):(nrow(all)),]
 
fm.more <- type ~ bone_length+rotting_flesh+hair_length+has_soul+color+bone_rot+bone_hair+ bone_soul+rot_hair+rot_soul+hair_soul+rot2+hair2+soul2+bone2
 
 
 tc <- rpart.control(minsplit=20,minbucket=10,maxdepth=10,xval=5,cp=0.005)
# 训练新的模型
mod.more <- rpart(formula=fm.more, data=train, method="class", control=tc)
pred.more <-predict(mod.more, test, type='class')
 
res <- data.frame(id = test$id, type = pred.more)
res
write.csv(res, file = "rpart3.csv", row.names = FALSE)

# 71.455mod.rf <- train(fm.more, data = train,  method = "rf",  trControl = ctrl,  tuneLength = 3)
#71.077mod.ranger=ranger(fm.more,data=train,num.trees = 3000,num.threads = 8,classification = T)
mod.cforest=cforest(fm.more,train,controls=cforest_unbiased(ntree=1500, mtry=6))
####randomForest
pred.rf <-predict(mod.cforest, test,OOB = T)

# 生成结果
res <- data.frame(id = test$id, type = pred.rf)
write.csv(res, file = "cforest.csv", row.names = FALSE)














