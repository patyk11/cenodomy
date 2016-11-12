source('./models_random_forest/rf_load_packages.R')
source('./models_random_forest/rf_load_data.R')

# Deal with missing values
variables <- names(train)
variables <- variables[variables != "SalePrice"]
print(variables)
for(variable in variables)
{
  if(any(is.na(train[[variable]])))
  {
    print(paste(variable,"-",class(train[[variable]])))
    if(is.character(train[[variable]]))
    {
      train[[variable]][is.na(train[[variable]])] <- "Missing"
    }
    else
    {
      
      varFit <- rpart(as.formula(paste(variable, '~ SalePrice + YearBuilt + `X1stFlrSF` + MSSubClass')),
                      data=train[!is.na(train[, variable]),], 
                      method="anova")
      train[, variable][is.na(train[, variable])] <- predict(varFit, train[is.na(train[, variable]),])
      
      # rest of variables
      train[[variable]][is.na(train[[variable]])] <- mean(train[[variable]],na.rm=TRUE)
      
    }
  }
  if(any(is.na(test[[variable]])))
  {
    if(is.character(test[[variable]]))
    {
      test[[variable]][is.na(test[[variable]])] <- "Missing"
    }
    else
    {
      varFit <- rpart(as.formula(paste(variable, '~ YearBuilt + `X1stFlrSF` + MSSubClass')),
                      data=test[!is.na(test[, variable]),], 
                      method="anova")
      test[, variable][is.na(test[, variable])] <- predict(varFit, test[is.na(test[, variable]),])
      
      # rest of variables
      test[[variable]][is.na(test[[variable]])] <- mean(test[[variable]],na.rm=TRUE)
    }
  }
}

# Deal with factors
for(variable in variables)
{
  if(is.character(train[[variable]]))
  {
    levels <- sort(unique(c(train[[variable]],test[[variable]])))
    train[[variable]] <- factor(train[[variable]],levels=levels)
    test[[variable]] <- factor(test[[variable]],levels=levels)
  }
}

rf <- randomForest(SalePrice~.,train,
                   do.trace=TRUE)

p <- predict(rf,test)
submission$SalePrice <- p
write.csv(submission,file=sprintf("./out/submission_%s.csv", Sys.Date()),row.names=FALSE)
