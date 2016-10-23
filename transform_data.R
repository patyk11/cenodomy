
DT_train <- DT_train[, lapply(.SD, as.character)]

numeric_cols <- c(  'LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF2', 'BsmtUnfSF'
                    , 'TotalBsmtSF', '1stFlrSF', '2ndFlrSF', 'GrLivArea', 'GarageArea'
                    , 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'SalePrice')
rest_cols <- setdiff(names(DT_train), numeric_cols)

DT_train <- cbind(
  DT_train[, lapply(.SD, as.numeric)
           , .SDcols = numeric_cols]
  , DT_train[, rest_cols, with = FALSE]
)
