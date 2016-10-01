library(data.table)
library(ggplot2)
library(randomForest)
library(RRF)

DT_cleaned<-as.data.table( cbind(X_train,y))


sample_size=200
indexes_test <- sample(nrow(DT_cleaned), sample_size)

DT_train <- DT_cleaned[!indexes_test]
DT_test <- DT_cleaned[indexes_test]

rf <- randomForest(x=DT_train[ ,!'y', with = FALSE], y=DT_train[, y], 
                   ntree=1000, importance=TRUE,maxnodes = 15, na.action=na.omit,keep.forest=TRUE)


realScores <- data.table(real = DT_test$y)

predictedScores <- predict(rf, DT_test[, !'y', with=F])



SSE = sum((predictedScores - realScores)^2)
SSE

imp <- rf$importance


featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
featureImportance1<-featureImportance[featureImportance$Importance>0.001,]

p <- ggplot(featureImportance1, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

p
