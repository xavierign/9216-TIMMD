
#### 1. libraries and clean ####
rm(list=ls())
cat("\f")
library(rpart)
library(rpart.plot)
library(geosphere)
library(caret)
set.seed(1)

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

#### 4 define new features #### in both test and train
# verifies NAs in Ammount and mark them
df_train$AmmountNA <- F
df_train$AmmountNA[is.na(df_train$Ammount)] <- T

df_test$AmmountNA <- F
df_test$AmmountNA[is.na(df_test$Ammount)] <- T

# generates n_points randomly in an area central to the data
# then calculates their distance to the observation
# this is for 'proyecting' the points
# to explore:
# boxplot(df_train$LAT)

n_points = 5
range_LATs <- c(39.26,39.29)
range_LONGs <- c(-76.73,-76.70)

for (i in 1:n_points) {
  ref_name <- paste('dist_point_',i,sep="")
  
  Lat1 <- runif(1, range_LATs[1], range_LATs[2])
  Long1 <- runif(1, range_LONGs[1], range_LONGs[2])
  
  #distHaversine see 'help(distHaversine)'
  df_train[,ref_name] <- distHaversine(c(Long1,Lat1),matrix(c(df_train$LONG, df_train$LAT),ncol=2))
  df_test[,ref_name] <- distHaversine(c(Long1,Lat1),matrix(c(df_test$LONG, df_test$LAT),ncol=2))
}

#cuts the largest numbers in Ammount. within the 95 quartile
thres <- quantile(df_train$Ammount, 0.95,na.rm=T)
ids_to_mark <- df_train$Ammount > thres
df_train$Ammount[ids_to_mark] <- thres
df_train$AmmountEdited <- F
df_train$AmmountEdited[ids_to_mark] <- T

# marks the observations with Ammount edited
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
# cost confituration with cost matrix
loss_matr <- matrix(c(0, 1, 2, 0), nrow = 2)
fit <- rpart(Target~., data=df_train,cp=0.0005,parms = list(loss = loss_matr))
rpart.plot(fit)

#### 5. use the model to predict ####
out <- predict(fit, newdata = df_test, type='class')
df_out <- data.frame(id=rownames(df_test), prediction=out) #id and prediction must be the col.names
write.csv(df_out,file='data/out_tree.csv',row.names =F)

# cost obtained 1917 quite good!

#### 5. prepare data for xgboost ####
# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
library(xgboost)

# for xgboost algorithm all variables must be numeric.
# To prepere the variables is a good practice get one
# dataset for all train and test and do exactly the same process.

# df_tot: last 15000 rows are test, other first are train
df_tot <- rbind(df_train[,!colnames(df_train) %in% c("Target")],
                df_test)

# lets explore
summary(df_tot)

# factor variables are f1, f2, weekday
# these need to be numeric fo xgboost
factor_vars <- c('f1','f2','Weekday')
for (var in factor_vars) {
  df_tot[,var] <- as.factor(df_tot[,var])
  print(c(var, length(levels(df_tot[,var]))))
}

# f2 has 90 different categories, those need to be reduced 
# how many obs in each category
table(df_tot$f2)

# we will append 5 (n_cols_for_f2) columns for this category
n_cols_for_f2 <-5
col_names_for_f2 <- names(sort(table(df_tot$f2), decreasing = T)[1:n_cols_for_f2])
df_tot$f2_trimmed <- as.character(df_tot$f2)
df_tot$f2_trimmed[!df_tot$f2 %in% col_names_for_f2] <- 'other'
df_tot$f2_trimmed <- as.factor(df_tot$f2_trimmed)

library('fastDummies')
# https://www.marsja.se/create-dummy-variables-in-r/

df_XG <- dummy_cols(df_tot, select_columns = c('f1', 'f2_trimmed', 'Weekday'),
           remove_selected_columns = TRUE)

cols_to_remove <- c('f2','Date')

df_XG <- df_XG[,!colnames(df_XG) %in% cols_to_remove]

cols_to_numeric <- c('AmmountNA','AmmountEdited')
df_XG[,cols_to_numeric] <- df_XG[,cols_to_numeric] + 0

summary(df_XG)

#### prepare for training. Input data needs to be numeric in a matrix
df <- as.matrix(df_XG[1:(nrow(df_XG)-nrow(df_test)),])
labels = as.matrix(df_train$Target)

ids_train <- sample(c(T,F),nrow(df),prob = c(0.7,0.3),replace = T)

x_train <- df[ids_train,]
y_train <- labels[ids_train]
 
x_test <- df[!ids_train,]
y_test <- labels[!ids_train]

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

watchlist <-list(eval = dtest, train = dtrain)

##### Q1 ) whrere this 1/3 cames from? obtain formula
thres <- 1/3

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds <- preds>thres
  false_positives <- sum(preds &!labels)
  false_negatives <- sum(!preds &labels)
  return(list(metric = "error", value = nrow(df_test)/length(labels)*(false_positives + 2*false_negatives)))
}

param <- list(max_depth = 7, eta = 0.1, nthread  =  1, verbosity = 1,
              objective = "binary:logistic", eval_metric = evalerror) #logregobj) #
nrounds = 50
bst <- xgb.train(param, dtrain, nrounds = nrounds, watchlist,early_stopping_rounds = 10,
                 maximize =F)

#### 5. use the model to predict ####
preds_probs <- predict(bst, dtrain)
conf_mat <- confusionMatrix(as.factor(as.numeric(preds_probs>thres)), 
                as.factor(as.numeric(y_train)))
conf_mat$table

#### 6. use the model to predict the ids and send the file to score
pred_df_XG <- as.matrix(df_XG[(nrow(df_XG) -nrow(df_test)+1):nrow(df_XG),])
pred <- rep(0,nrow(df_test))
pred[predict(bst, pred_df_XG)>thres] <- 1

df_out <- data.frame(id=rownames(df_test), prediction=pred) #id and prediction must be the col.names
write.csv(df_out,file='data/out_xg.csv',row.names =F)
# cost obtained 1821

### train 55 rounds with all data available:
nrounds = 55

dtrain <- xgb.DMatrix(data = rbind(x_train,x_test), label = c(y_train,y_test))
bst <- xgb.train(param, dtrain, nrounds = nrounds, watchlist,early_stopping_rounds = 10,
                 maximize =F)

#### 5. use the model to predict ####

#### 6. use the model to predict the ids and send the file to score
pred_df_XG <- as.matrix(df_XG[(nrow(df_XG) -nrow(df_test)+1):nrow(df_XG),])
pred <- rep(0,nrow(df_test))
pred[predict(bst, pred_df_XG)>thres] <- 1

df_out <- data.frame(id=rownames(df_test), prediction=pred) #id and prediction must be the col.names
write.csv(df_out,file='data/out_xg_all.csv',row.names =F)
# cost obtained 1810

