
# 1 Setting up --------------------------------------------------------------

# 1.1 Loading packages 

library(pacman)
p_load(plyr, readr, dplyr, ggplot2, repr, Rcpp, caret, randomForest, glmnet)

# 1.2 Importing data

dat_import <- readRDS('output_data/features_final.rds') 

# Check for NAs 
colnames(dat_import)[colSums(is.na(dat_import)) > 0]

# Remove rows containing NAs
dat <- dat_import[complete.cases(dat_import), ]

dat <- dat %>% 
  mutate(offense_defense = ifelse(offense_defense=="offense", 1, 0)) %>% 
  dplyr::select(-X, -ball_start_x, -ball_end_x)

glimpse(dat)

# Check for correlation between features 
dat_matrix <- dat %>% 
  dplyr::select(-perf_score, -matchId, -half, -phase_id)

source("function_headers.r")

matrix_pearson <- cor(dat_matrix, method = c("pearson"))

p_load(corrplot)
corrplot(matrix_pearson, method="circle", tl.col="black", , tl.srt = 45, tl.cex = .6)

png(file="../output/corrmatrix.png",
    width=600, height=600)
corrplot(matrix_pearson, method="circle", tl.col="black", , tl.srt = 45, tl.cex = .65)


dat_matrix <- dat %>% 
  dplyr::select(-perf_score, -matchId, , -half, -phase_id, -passes_min_mean, -n_passes_total, -sd_passes_player, -max_abs_alignment_dir, -sd_cent_pos, -max_cent_pos, -max_cent_dir, -max_cent_speed, -max_cent_pass, -sd_cent_pass, -mean_cent_dir, -mean_cent_speed) 

matrix_pearson <- cor(dat_matrix, method = c("pearson"))
corrplot(matrix_pearson, method="circle", tl.col="black", tl.srt = 45, tl.cex = .7)

dat <- dat %>%  
  dplyr::select(-half, -passes_min_mean, -n_passes_total, -sd_passes_player, -max_abs_alignment_dir, -sd_cent_pos, -max_cent_pos, -max_cent_dir, -max_cent_speed, -max_cent_pass, -sd_cent_pass, -mean_cent_dir, -mean_cent_speed) 


# 2 Data partitioning -------------------------------------------------------

# Random sample from each unique match
set.seed(1234) 
unique_matches <- unique(dat$matchId)
sample_test <- as.list(sample(unique_matches, round(length(unique_matches)*0.7,0)))

train = dat %>%  
  filter(matchId %in% sample_test) %>%  
  select(-matchId, -phase_id)

test <- dat %>% 
  filter(!(matchId %in% sample_test)) %>%  
  select(-matchId, -phase_id)

dim(train)
dim(test)

# 3 Normalization ---------------------------------------------------------

# Scaling 
cols_scale = colnames(train)[-length(train)] # all columns except target variable 

pre_proc_val <- preProcess(train[,cols_scale], method = c("center", "scale"))

train[,cols_scale] = predict(pre_proc_val, train[,cols_scale])

test[,cols_scale] = predict(pre_proc_val, test[,cols_scale])

summary(train)
summary(test)

# Regularization 
cols_reg = colnames(train) # all columns  

dummies <- dummyVars(perf_score ~ ., data = dat[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))

# Check for correlation between features 
cor(train, method = c("spearman"))


# 4 Feature reduction (Lasso regression) -------------------------------------------------------

# Finding the best lamda

x = as.matrix(train_dummies)
y_train = train$perf_score

x_test = as.matrix(test_dummies)
y_test = test$perf_score

lambdas <- 10^seq(2, -3, by = -.1)

# alpha=.5 = elastic net
cv.lasso <- cv.glmnet(x, y_train, alpha = .5, lambda = lambdas, standardize = F, nfolds = 5)
plot(cv.lasso)

png(file="../output/perf_segment_lasso_cv_lambda.png",
    width=600, height=350)
plot(cv.lasso)
dev.off()

# Bestlambda
lambda_best <- cv.lasso$lambda.min 
lambda_best

# Lasso regression 
lasso_model <- glmnet(x, y_train, alpha = .5, lambda = lambda_best)
df_coef <- round(as.matrix(coef(lasso_model)), 3)

# See the non-contributing values (coef=0)
write.csv(df_coef, "../output/perf_segment_lasso_coefficients.csv")
zero_features <- names(df_coef[df_coef[, 1] == 0, ])

# Feature coefficients after removing non-contributing values 
df_coef <- df_coef[df_coef[, 1] != 0, ]

# Copy datasets to keep full data 
train_all_features <- train 
test_all_features <- test 

# Remove zero values 
train <- train[ , -which(names(train) %in% zero_features)]
test <- test[ , -which(names(test) %in% zero_features)]


# 5 Model training ----------------------------------------------------------

#---- Models ----#

# Set up a 5-fold cross validation
control <- trainControl(method="repeatedcv", number=5, repeats=3)
metric <- "RMSE"

# Source: https://towardsdatascience.com/machine-learning-with-r-linear-regression-558fa2edaaf0
# http://zevross.com/blog/2017/09/19/predictive-modeling-and-machine-learning-in-r-with-the-caret-package/

# Running default models  
lm1_default <- train(perf_score~., data = train, method = "lm", metric=metric, trControl=control)
rf1_default <- randomForest(perf_score~., data = train, metric=metric, trControl=control)


# 6 Parameter tuning ------------------------------------------------------

# Source: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://towardsdatascience.com/hyperparameter-tuning-the-random-forest-in-python-using-scikit-learn-28d2aa77dd74 

# -- mtry ---#

control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
set.seed(1234)
mtry <- sqrt(ncol(x))

# Tuning - random search 
rf_random <- train(perf_score~., data=train, method="rf", metric="RMSE", tuneLength=5, trControl=control)
print(rf_random)
plot(rf_random)

png(file="../output/perf_segment_mtry_tuning_random.png",
    width=600, height=350)
plot(rf_random)
dev.off()

# Best mtry acc. to random search is mtry=9  (lowest RSME)

save.image("temp_workspace_ML_2204.RData")


# -- Custom tuning -- # 
# -- mtry AND ntree---#

# Manual Search
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")

# Grid search extended caret: 
# Extend caret 
customRF <- list(type= "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


# train model
control <- trainControl(method="repeatedcv", number=5, repeats=3)
tunegrid <- expand.grid(.mtry=c(2,5,10,12,15,18,20), .ntree=c(200, 400, 800, 1000, 2000))
set.seed(1234)
custom <- train(perf_score~., data=train, method=customRF, metric="RMSE", tuneGrid=tunegrid, trControl=control)
summary(custom)

png(file="../output/perf_segment_custom_parameter_tuning_ntree_mtry.png",
    width=600, height=350)
plot(custom)
dev.off()  

plot(custom)

control <- trainControl(method="repeatedcv", number=5, repeats=3)
metric <- "RMSE"
set.seed(1234)

ntree=400
mtry=15


# 7 Model training --------------------------------------------------------
fit.rf <- randomForest(perf_score~., data=train, metric=metric, trControl=control, ntree=ntree, mtry=mtry)
fit.rf_all_features <- randomForest(perf_score~., data=train_all_features, metric=metric, trControl=control, ntree = ntree, mtry=mtry)
rf1_default_all_features <- randomForest(perf_score~., data = train_all_features, metric=metric, trControl=control)

lm1_default_all_features <- train(perf_score~., data = train_all_features, method = "lm", metric=metric, trControl=control)
lm1_default_selected_features <- train(perf_score~., data = train, method = "lm", metric=metric, trControl=control)


# 8 Model performance -----------------------------------------------------------
set.seed(1234)

# Tuned model with selected features 
predictions_tuned_model <- predict(fit.rf, test)
RMSE(predictions_tuned_model,test$perf_score)

# Tuned model with all features 
predictions_all_features <- predict(fit.rf_all_features, test_all_features)
RMSE(predictions_all_features,test_all_features$perf_score)

# Default model with selected features:
predictions_default <- predict(rf1_default, test)
RMSE(predictions_default,test$perf_score)

# Default model with all features 
predictions_default_all_features <- predict(rf1_default_all_features, test_all_features)
RMSE(predictions_default_all_features,test_all_features$perf_score)

#----- multiple linear regression models -----# 
# Default model with all features 
lm_predictions_all_features <- predict(lm1_default_all_features, test_all_features)
RMSE(lm_predictions_all_features,test_all_features$perf_score)

# Default model with selected features 
lm_predictions_selected_features <- predict(lm1_default_selected_features, test_all_features)
RMSE(lm_predictions_selected_features,test_all_features$perf_score)

# Tuned model with selected features 
predictions_list <- list(fit.rf_all_features)
predictions_df <- data.frame(pred = unlist(predictions_list)) %>% 
  cbind(test[,c("perf_score")])

saveRDS(predictions_df, "predictions_df.rds")

# 9. Save image -----------------------------------------------------------
save.image('3_ml_final') 

