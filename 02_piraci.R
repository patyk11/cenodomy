library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(moments)
library(glmnet)
library(elasticnet)
library(knitr)


train<-  read.csv("dane/train(1).csv", stringsAsFactors = F)
test<- read.csv("dane/test(1).csv", stringsAsFactors = F)


all_data <- rbind(select(train,MSSubClass:SaleCondition),
                  select(test,MSSubClass:SaleCondition))

# ploting histograms of price and logprice
df <- rbind(data.frame(version="log(price+1)",x=log(train$SalePrice + 1)),
            data.frame(version="price",x=train$SalePrice))

# plot histogram
ggplot(data=df) +
  facet_wrap(~version,ncol=2,scales="free_x") +
  geom_histogram(aes(x=x))


# transform SalePrice target to log form
train$SalePrice <- log(train$SalePrice + 1)

# for numeric feature with excessive skewness, perform log transformation
# first get data type for each feature
feature_classes <- sapply(names(all_data),function(x){class(all_data[[x]])})
numeric_feats <-names(feature_classes[feature_classes != "character"])

# determine skew for each numeric feature
skewed_feats <- sapply(numeric_feats,function(x){skewness(all_data[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  all_data[[x]] <- log(all_data[[x]] + 1)
}



# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical features
dummies <- dummyVars(~.,all_data[categorical_feats])
categorical_1_hot <- predict(dummies,all_data[categorical_feats])
categorical_1_hot[is.na(categorical_1_hot)] <- 0  #for any level that was NA, set to zero

# for any missing values in numeric features, impute mean of that feature
numeric_df <- all_data[numeric_feats]

for (x in numeric_feats) {
  mean_value <- mean(train[[x]],na.rm = TRUE)
  all_data[[x]][is.na(all_data[[x]])] <- mean_value
}

# reconstruct all_data with pre-processed data
all_data <- cbind(all_data[numeric_feats],categorical_1_hot)

# create data for training and test
X_train <- all_data[1:nrow(train),]
X_test <- all_data[(nrow(train)+1):nrow(all_data),]
y <- train$SalePrice




# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)



# test out Ridge regression model

lambdas <- seq(1,0,-0.001)

# train model
set.seed(124)  # for reproducibility
model_ridge <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))



ggplot(data=filter(model_ridge$result,RMSE<0.14)) +
  geom_line(aes(x=lambda,y=RMSE))


mean(model_ridge$resample$RMSE)



# train  lassomodel
set.seed(123)  # for reproducibility
model_lasso <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
model_lasso

mean(model_lasso$resample$RMSE)
