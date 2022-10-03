
#### 1. libraries and clean ####
rm(list=ls())
cat("\f")
library(rpart)
library(rpart.plot)
library(geosphere)

#### 2. read data ####
df_train <- read.csv('data/df_train.csv')
rownames(df_train) <- df_train$X
df_test <- read.csv('data/df_test.csv')
rownames(df_test) <- df_test$X

#### 3. treat data ####
df_train$Date <- as.POSIXlt(df_train$Date)
df_test$Date <- as.POSIXlt(df_test$Date)
df_train$Target <- as.factor(df_train$Target)

df_train$X <- NULL
df_test$X <- NULL

#### 5. define new features #### in both test and train
df_train$AmmountNA <- F
df_train$AmmountNA[is.na(df_train$Ammount)] <- T

df_test$AmmountNA <- F
df_test$AmmountNA[is.na(df_test$Ammount)] <- T

boxplot(df_train$LAT)
range_LATs <- c(39.26,39.29)
range_LONGs <- c(-76.73,-76.70)

n_points = 5

for (i in 1:n_points) {
  ref_name <- paste('dist_point_',i,sep="")
  
  Lat1 <- runif(1, range_LATs[1], range_LATs[2])
  Long1 <- runif(1, range_LONGs[1], range_LONGs[2])
  
  df_train[,ref_name] <- distHaversine(c(Long1,Lat1),matrix(c(df_train$LONG, df_train$LAT),ncol=2))
  df_test[,ref_name] <- distHaversine(c(Long1,Lat1),matrix(c(df_test$LONG, df_test$LAT),ncol=2))
}

thres <- quantile(df_train$Ammount, 0.95,na.rm=T)
ids_to_mark <- df_train$Ammount > thres
df_train$Ammount[ids_to_mark] <- thres
df_train$AmmountEdited <- F
df_train$AmmountEdited[ids_to_mark] <- T

ids_to_mark <- df_test$Ammount > thres
df_test$Ammount[ids_to_mark] <- thres
df_test$AmmountEdited <- F
df_test$AmmountEdited[ids_to_mark] <- T

## dates
df_train$Month <- as.numeric(format(df_train$Date,format="%m"))
df_train$Weekday <- weekdays(df_train$Date)
df_train$Hour <- as.numeric(format(df_train$Date,format="%H"))
df_train$Date <- as.Date(df_train$Date)

df_test$Month <- as.numeric(format(df_test$Date,format="%m"))
df_test$Weekday <- weekdays(df_test$Date)
df_test$Hour <- as.numeric(format(df_test$Date,format="%H"))
df_test$Date <- as.Date(df_test$Date)


#### 4. fit tree ####
loss_matr <- matrix(c(0, 1, 2, 0), nrow = 2)
fit <- rpart(Target~., data=df_train,cp=0.0005,parms = list(loss = loss_matr))
rpart.plot(fit)

#### 5. use the model to predict ####
out <- predict(fit, newdata = df_test, type='class')
df_out <- data.frame(id=rownames(df_test), prediction=out) #id and prediction must be the col.names
write.csv(df_out,file='data/out_n10.csv',row.names =F)
