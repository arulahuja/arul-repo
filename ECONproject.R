library(magrittr)
library(caret)
library(glmnet)
require(MASS)
library(ISLR)
library(moments)
library(stats)
library(tidyverse)
library(DoubleML)
library(mlr3)
library(class)
library(nnet)
library(ranger)
library(data.table)
library(hdm)
library(xtable)
library(stargazer)



df <- read.csv('/Users/arulahuja/Desktop/Econ484/Sales_House_Data.csv')
df <- df %>% select(-c('X'))
df <- df %>% mutate(DocumentDate = as.Date(DocumentDate) )
# Only do where the sales is for land with new building, used building, or family home
df <- df %>% filter(PropertyType %in% c(2,3,4,10,11,12))
# Filter out any none residential use
df <- df %>% filter(PrincipalUse == 6)
# Filter out deed that only have a fair amount of data
df <- df %>% filter(SaleInstrument %in% c(2,3,4,15,18,24,26,27))
# Filter out sales reason such as gift
df <- df %>% filter(SaleReason %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))
# Filter only the residential property
df <- df %>% filter(PropertyClass == 8)
#Code I use to further clean the data
# Mutate Variable to factors if needed
df <- df %>% mutate(PropertyType = factor(PropertyType), SaleInstrument = factor(SaleInstrument), SaleReason = factor(SaleReason), HeatSystem = factor(HeatSystem), HeatSource = factor(HeatSource), Condition = factor(Condition), DistrictName = factor(DistrictName), InadequateParking = factor(InadequateParking), TrafficNoise = factor(TrafficNoise), AirportNoise = factor(AirportNoise) )
# Get sales price into log
df <- df %>% mutate(SalePriceLog = log(SalePrice))
# Get Mutataing and Lake view
df <- df %>% mutate(MountainView = ((MtRainier>0) |(Olympics>0)|(Cascades >0) | (Territorial >0)),
                    WaterView = ((PugetSound>0)|(LakeWashington > 0)|(LakeSammamish >0)|(SmallLakeRiverCreek >0)))
# Get Year and month variable
df <- df %>% mutate(Year = as.integer(format(DocumentDate,'%Y')), Month = format(DocumentDate,'%m'))
# Add var together
df <- df %>% mutate(SqFtGarage = SqFtGarageAttached+SqFtGarageBasement, Bath = BathHalfCount*0.5 + Bath3qtrCount*0.75 + BathFullCount, SqFtPorche = SqFtEnclosedPorch + SqFtOpenPorch, YrSinceBuilt = Year - YrBuilt, YrSinceConstrc = Year - LastConstruction )
#Filter out where sale price is less than $5.
df <- df %>% filter(SalePrice > 5)
#Filtering out year
df <- df %>% filter(Year >= 2019)
#Add Variable for Stories:
df <- df %>% mutate(MoreThan1Story = ifelse(Stories>1, 1, 0))
#Get Appraisal
apraisal = function(df1_r){
  return(as.integer(df1_r[paste('X',df1_r['Year'],'.ImpsVal',sep='')]) + as.integer(df1_r[paste('X',df1_r['Year'],'.LandVal',sep='')]))
}
df$Appraisal <- apply(df,1,apraisal)



#Make Dataframe
df2 <- data.frame(df)
df2 <- select(df, -c(identifier, Major, Minor, DocumentDate, RecordingNbr, X2022.LandVal, 
                     X2022.ImpsVal, DistrictName, YrBuilt, LastConstruction, SqFtGarageAttached, 
                     SqFtGarageBasement, BathHalfCount, Bath3qtrCount, BathFullCount, SqFtEnclosedPorch, SqFtOpenPorch,
                     ZipCode, Stories, CurrentZoning, PugetSound, LakeSammamish, 
                     LakeWashington, SmallLakeRiverCreek, Territorial, MtRainier, Olympics, Cascades, SalePrice,
                     PropertyClass, PrincipalUse, Exist2ndFloor, Year, Month))
df2 <- df2[complete.cases(df2), ]
y <- df2$SalePriceLog

##CONVERTING TO NUMERICS
indx2 <- sapply(df2, is.character)
df2[indx2] <- lapply(df2[indx2], function(x) as.factor(x))
#indx <- sapply(df2, is.factor)
#df2[indx] <- lapply(df2[indx], function(x) as.numeric(factor(as.matrix(x))))



####
#fbnew = list("Stories", "YrSinceBuilt", "YrSinceConstrc", "Year", "Appraisal", "MountainView", "WaterView", "SqFtLot", 'ExistGarage', "Range", "BathFullCount", "HeatSource")
#fbnew2 = (unlist(fbnew))




fm1<-as.formula(~-1+SalePriceLog+.)
X2 <- model.matrix(fm1, data = df_ols)
X2 <- X2[, which(apply(X2, 2, var) != 0)] 
df3 = as.data.frame(X2)
fbnew3 = names(df3)[names(df3)!='SalePriceLog' & names(df3)!='MoreThan1Story']


formula_ML <- as.formula(~ -1+ SalePriceLog+SaleReason+YrRenovated+Condition+Bath+
                                  SqFt2ndFloor+SqFtTotLiving+HeatSource+ExistPorch+WaterSystem +
                                  TrafficNoise+InadequateParking+MountainView+WaterView+YrSinceBuilt+MoreThan1Story)
X <- model.matrix(formula_ML, df2)
X <- X[, which(apply(X, 2, var) != 0)] 
Tree_ML_df = as.data.frame(X)
fbnew2 = names(Tree_ML_df)[names(Tree_ML_df)!='SalePriceLog' & names(Tree_ML_df)!='MoreThan1Story']



#features_base <- colnames(df2)[!colnames(df2) %in% c('ExistPorch','SalePriceLog')]
data_dml_base2 <- DoubleMLData$new(Tree_ML_df,y_col = 'SalePriceLog', d_cols = 'MoreThan1Story',x_cols = fbnew2)
#First use a lasso as learner
set.seed(9432)
lasso <- lrn("regr.cv_glmnet", nfolds = 5, s = "lambda.min")
lasso_class <- lrn("classif.cv_glmnet", nfolds = 5, s = "lambda.min")
dml_plr_lasso1 = DoubleMLPLR$new(data_dml_base2,
                                 ml_g = lasso,
                                 ml_m = lasso_class,
                                 n_folds = 3)
dml_plr_lasso1$fit()
randomForest <-lrn("regr.ranger", max.depth = 7,
                   mtry = 20, min.node.size = 3)
randomForest_class <- lrn("classif.ranger", max.depth = 5,
                          mtry = 4, min.node.size = 7)
# Use a random forest as a learner
dml_plr_forest2 <- DoubleMLPLR$new(data_dml_base2,
                                   ml_g = randomForest,
                                   ml_m = randomForest_class,
                                   n_folds = 3)
dml_plr_forest2$fit()


data_dml_base3 <- DoubleMLData$new(df3,y_col = 'SalePriceLog', d_cols = 'MoreThan1Story',x_cols = fbnew3)
dml_plr_forest3 <- DoubleMLPLR$new(data_dml_base3,
                                   ml_g = randomForest,
                                   ml_m = randomForest_class,
                                   n_folds = 3)
dml_plr_forest3$fit()

confints = rbind(dml_plr_lasso1$confint(), dml_plr_forest2$confint())
estimates = c(dml_plr_lasso1$coef, dml_plr_forest2$coef)
result_plr = data.table("model" = "PLR",
                        "ML" = c("glmnet", "ranger"),
                        "Estimate" = estimates,
                        "lower" = confints[,1],
                        "upper" = confints[,2])
result_plr


## DOUBLE LASSO FOR SELECTION
df_ols = df2[, (sapply(df2, nlevels)>1) | (sapply(df2, is.factor)==FALSE)]

OLS = lm(SalePriceLog ~., data = df_ols)
lasso = rlasso(SalePriceLog~., data = df_ols, post = FALSE) # = Run the Rigorous LASSO = #
selected = which(coef(lasso)[-c(1:2)] !=0) #Select features that aren't reduced to zero

#MAKE MATRIX FOR SELECTED
formula_vocal = as.formula(~ -1 + MoreThan1Story + MoreThan1Story:(SaleReason+YrRenovated+Condition+Bath+
                                                                     SqFt2ndFloor+SqFtTotLiving+HeatSource+ExistPorch+WaterSystem
                                                                   +TrafficNoise+InadequateParking+MountainView+WaterView+YrSinceBuilt) + (SaleReason+YrRenovated+Condition+Bath+
                                                                                                                                             SqFt2ndFloor+SqFtTotLiving+HeatSource+ExistPorch+WaterSystem +
                                                                                                                                             TrafficNoise+InadequateParking+MountainView+WaterView+YrSinceBuilt))
vocal_matrix = model.matrix(formula_vocal, data=df_ols)                                                                                     
vocal_matrix <- vocal_matrix[, which(apply(vocal_matrix, 2, var) != 0)]
vocmatrixdf <- as.data.frame(vocal_matrix)
olsmatrix <- model.matrix(SalePriceLog~., data = df_ols)
olsmatrix <- olsmatrix[, which(apply(olsmatrix, 2, var) != 0)] 
olsmatrixdf = as.data.frame(olsmatrix)

index.floor <- grep("MoreThan1Story", colnames(vocmatrixdf))

effects.dl <- rlassoEffects(x=vocmatrixdf, y=df_ols$SalePriceLog, index = index.floor, method="double selection")
#effects.dl <- rlassoEffects(x=olsmatrixdf[, names(olsmatrixdf)!='SalePriceLog'], y=df_ols$SalePriceLog, method="double selection", index = selected)
selected2 = which(coef(effects.dl)[-c(1:2)] !=0)
selected2                 
summary(effects.dl)
pvals.effects.dl <- p_adjust(effects.dl)
pvals.effects.dl



