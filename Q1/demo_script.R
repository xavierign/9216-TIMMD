
#### 1. libraries and clean ####
rm(list=ls())
cat("\f")
library(rpart)
library(rpart.plot)

#### 2. read data ####
df_train <- read.csv('data/df_train.csv')
rownames(df_train) <- df_train$X
df_test <- read.csv('data/df_test.csv')
rownames(df_test) <- df_test$X

#### 3. treat data ####
df_train$Date <- as.Date(df_train$Date)
df_test$Date <- as.Date(df_test$Date)
df_train$Target <- as.factor(df_train$Target)

df_train$X <- NULL
df_test$X <- NULL

summary(df_train)
#### 4. fit tree ####
fit <- rpart(Target~., data=df_train,cp=0.001)
rpart.plot(fit)

#### 5. use the model to predict ####
out <- predict(fit, newdata = df_test, type='class')
df_out <- data.frame(id=rownames(df_test), prediction=out) #id and prediction must be the col.names
write.csv(df_out,file='data/out.csv',row.names =F)
