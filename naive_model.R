lm_full <- lm(formula = SalePrice ~., data = select(DT_train, c(LotFrontage:Street, LotShape:LandContour
                                                     , LotConfig:PoolArea, MiscVal:SaleCondition)))

# a lot of variables, probably completely no sense

lm_full_summary <- summary(lm_full)


lm_small <- lm(  formula = SalePrice ~ OverallQual + GrLivArea + YearBuilt + GarageCars
               , data = select(DT_train, c(  LotFrontage:Street
                                           , LotShape:LandContour
                                           , LotConfig:PoolArea
                                           , MiscVal:SaleCondition)))
