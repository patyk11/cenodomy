nvmax <- 6

DT_subset <- dplyr::select(DT_train, c(  LotFrontage:Street # variables from lm_full
                            , LotShape:LandContour
                            , LotConfig:PoolArea
                            , MiscVal:SaleCondition))

step_model <- leaps::regsubsets(  SalePrice ~.
                    , data = DT_subset
                  , method = 'backward', nvmax  = nvmax)

step_model_summary <- summary(step_model)

# extracting variables
variables_chosen <- colnames(step_model_summary$which)[step_model_summary$which[nvmax + 1, ] == TRUE]
variables_chosen <- setdiff(variables_chosen, '(Intercept)')
# without ID
variables_chosen <- variables_chosen[!variables_chosen %like% 'Id']

# creating model
formula <- as.formula(paste0("SalePrice~", paste0(variables_chosen, collapse = '+')))
lm_regsubsets <- lm(formula, data = DT_subset)

# on test set

pred <- predict(lm_regsubsets, DT_test)

# path, what with this two na? why they are present?
  pred[is.na(pred)] <- median(pred, na.rm = TRUE)

DT_out <- cbind(DT_test[, .(Id)], as.data.table(pred))
setnames(DT_out, old = 'pred' , new = 'SalePrice')


write_csv(DT_out, path = sprintf('./out/sub%s.csv', Sys.Date()))
  